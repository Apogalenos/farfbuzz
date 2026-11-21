module Farfbuzz.Tests.AatKerxFormat4Tests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.AnchorTables
open Farfbuzz.Tests.LiteralDrive

/// <summary>The glyph count of the faces of these tests.</summary>
[<Literal>]
let private GlyphCount = 64

/// <summary>A null format 4 entry: no flags and no action.</summary>
let private nullEntry = { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0us }

/// <summary>Builds a one-state machine whose narrow class table maps glyph 0 to the user class 4.</summary>
/// <param name="actionEntry">The class 4 entry; the other four are null.</param>
/// <returns>The machine.</returns>
let private entryMachine (actionEntry: Entry) =
    let nClasses = 5
    let classBytes = Array.concat [ be16 0u; be16 1u; [| 4uy |] ]
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = 1; Wide = false }
      States = Sealed.init nClasses (fun i -> uint16 i)
      StateBias = 0
      RowCount = 1
      Entries = Sealed.init nClasses (fun i -> if i = 4 then actionEntry else nullEntry)
      IsExtended = false }

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

/// <summary>Builds a font over a face whose only table is an ankr table.</summary>
/// <param name="table">The ankr table bytes.</param>
/// <param name="xScale">The horizontal scale in units of 1/1000 em.</param>
/// <param name="yScale">The vertical scale in units of 1/1000 em.</param>
/// <returns>The font.</returns>
let private fontWith (table: byte array) (xScale: int) (yScale: int) =
    let file = sfnt 0x00010000u [ Tags.Ankr, table ]
    Font.withScale xScale yScale (Font.create (Face.withGlyphCount GlyphCount (Face.create (ReadOnlyMemory file) 0u)))

/// <summary>Builds a run of the given glyphs with their positions.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <param name="positions">The positions.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The run.</returns>
let private format4Run (glyphs: uint32 array) (positions: GlyphPosition array) (maxOps: int) =
    let count = glyphs.Length
    { GlyphRun.empty with
        Infos = Sealed.init count (fun i -> { Codepoint = glyphs[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
        Props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
        Positions = Sealed.init count (fun i -> positions[i])
        HavePositions = true
        MaxOps = maxOps
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>A position with a 100-unit advance and no offsets.</summary>
let private plainPosition = { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }

/// <summary>The attach type and chain of a position: the type in the third byte, the chain in the low two.</summary>
/// <param name="attachType">The attach type.</param>
/// <param name="chain">The chain.</param>
/// <returns>The <c>var</c> word.</returns>
let private attachVar (attachType: uint32) (chain: int) = (attachType <<< 16) ||| uint32 (uint16 chain)

/// <summary>The spec of a subtable whose action data is the given bytes.</summary>
/// <param name="actionType">The action type.</param>
/// <param name="body">The action data.</param>
/// <returns>The spec.</returns>
let private specOf (actionType: int) (body: byte array) =
    { TableBytes = ReadOnlyMemory body
      RangeEnd = body.Length
      ActionsAt = 0L
      ActionType = actionType }

/// <summary>The mark-then-act machine of the hand-built cases: glyph 0 marks on its first appearance and acts on the
/// second, and goes on marking.</summary>
let private markAndAct = entryMachine { NewState = 0; Flags = 0x8000us; Data0 = 0us; Data1 = 0us }

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

/// <summary>Builds a font over a face whose only table is a kerx table, with sixteen glyphs.</summary>
/// <param name="kerx">The kerx table bytes.</param>
/// <returns>The font.</returns>
let private kerxFont (kerx: byte array) =
    let file = sfnt 0x00010000u [ Tags.Kerx, kerx ]
    Font.create (Face.withGlyphCount 16 (Face.create (ReadOnlyMemory file) 0u))

/// <summary>The inputs of one literal format 4 run.</summary>
type private Input4 =
    { Literal: LiteralInput
      Spec: Format4Spec
      Model: Map<uint32, (int * int) array>
      Kind: AnkrLookupKind
      XScale: int
      YScale: int
      Horizontal: bool
      Reversed: bool }

/// <summary>The saturating sum of two ints.</summary>
/// <param name="a">The augend.</param>
/// <param name="b">The addend.</param>
/// <returns>The sum.</returns>
let private saturateAdd (a: int) (b: int) = int (Math.Clamp (int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

/// <summary>The saturating difference of two ints.</summary>
/// <param name="a">The minuend.</param>
/// <param name="b">The subtrahend.</param>
/// <returns>The difference.</returns>
let private saturateSub (a: int) (b: int) = int (Math.Clamp (int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

/// <summary>Reads a big-endian 16-bit value of a table.</summary>
/// <param name="table">The table.</param>
/// <param name="offset">The byte offset.</param>
/// <returns>The value.</returns>
let private u16At (table: ReadOnlyMemory<byte>) (offset: int) = BinaryCalls.u16 table.Span offset |> Farfbuzz.Harness.TailCalls.settle

/// <summary>A literal transliteration of <c>KerxSubTableFormat4::driver_context_t::transition</c>
/// (src/hb-aat-layout-kerx-table.hh:596-701), writing the position of the current glyph into the buffer as the C does, with
/// the ankr anchors read from the model the table was built from.</summary>
/// <param name="input">The input.</param>
/// <param name="font">The font.</param>
/// <param name="drive">The literal driver.</param>
/// <returns>The transition.</returns>
let private literalTransition (input: Input4) (font: Font) (drive: LiteralDrive) =
    let spec = input.Spec
    let markSet = ref false
    let mark = ref 0
    let anchorOf (glyph: uint32) (index: uint32) =
        match Map.tryFind glyph input.Model with
        | Some anchors when int index < anchors.Length -> anchors[int index]
        | _ -> (0, 0)
    let fits (at: int64) (size: int64) = at >= 0L && at + size <= int64 spec.RangeEnd
    fun (entry: Entry) ->
        let idx = drive.Idx
        let mutable returned = false
        if markSet.Value && entry.Data0 <> 0xFFFFus && idx < drive.Len then
            let positions = drive.Positions
            match spec.ActionType with
            | 0 ->
                let at = spec.ActionsAt + int64 entry.Data0 * 4L
                if fits at 4L then drive.MaxOps <- drive.MaxOps - 512
                returned <- true
            | 1 ->
                let at = spec.ActionsAt + int64 entry.Data0 * 4L
                if not (fits at 4L) then returned <- true
                else
                    let markPoint = uint32 (u16At spec.TableBytes (int at))
                    let currPoint = uint32 (u16At spec.TableBytes (int at + 2))
                    let (markX, markY) = anchorOf drive.Codepoints[mark.Value] markPoint
                    let (currX, currY) = anchorOf drive.Codepoints[idx] currPoint
                    let o = positions[idx]
                    positions[idx] <- { o with XOffset = saturateSub (Font.emScaleX font markX) (Font.emScaleX font currX); YOffset = saturateSub (Font.emScaleY font markY) (Font.emScaleY font currY) }
            | 2 ->
                let at = spec.ActionsAt + int64 entry.Data0 * 8L
                if not (fits at 8L) then returned <- true
                else
                    let word (k: int) = int (int16 (u16At spec.TableBytes (int at + 2 * k)))
                    let o = positions[idx]
                    positions[idx] <- { o with XOffset = saturateSub (Font.emScaleX font (word 0)) (Font.emScaleX font (word 2)); YOffset = saturateSub (Font.emScaleY font (word 1)) (Font.emScaleY font (word 3)) }
            | _ -> ()
            if not returned then
                let cross (index: int) = if input.Horizontal then positions[index].YOffset else positions[index].XOffset
                let mutable offset = cross mark.Value
                let mutable glyphPos = mark.Value
                let mutable count = 1
                let mutable walking = true
                while walking && count < drive.Len && (byte ((positions[glyphPos].Var >>> 16) &&& 0xFFu) &&& 2uy) <> 0uy do
                    let chain = int (int16 (positions[glyphPos].Var &&& 0xFFFFu))
                    if chain = 0 then walking <- false
                    else
                        let parent = uint32 (glyphPos + (if input.Reversed then -chain else chain))
                        if parent >= uint32 drive.Len then walking <- false
                        else
                            glyphPos <- int parent
                            offset <- saturateAdd offset (cross glyphPos)
                            count <- count + 1
                let o = positions[idx]
                let shifted = if input.Horizontal then { o with YOffset = saturateAdd o.YOffset offset } else { o with XOffset = saturateAdd o.XOffset offset }
                let var = (shifted.Var &&& ~~~0x00FF0000u) ||| 0x00010000u
                let mutable chain16 = int16 (mark.Value - idx)
                if input.Reversed then chain16 <- int16 (-(int chain16))
                positions[idx] <- { shifted with Var = (var &&& 0xFFFF0000u) ||| uint32 (uint16 chain16) }
                drive.Scratch <- drive.Scratch ||| uint32 ScratchFlags.HasGposAttachment
        if not returned && entry.Flags &&& 0x8000us <> 0us then
            markSet.Value <- true
            mark.Value <- idx

/// <summary>Generates a random format 4 input for one action type.</summary>
/// <param name="actionType">The action type.</param>
/// <returns>The generator.</returns>
let private inputGen (actionType: int) =
    gen {
        let! nClasses = Gen.elements [ 5; 6; 7 ]
        let! nStates = Gen.choose (1, 4)
        let entryGen =
            gen {
                let! newState = Gen.choose (0, nStates - 1)
                let! flags = Gen.elements [ 0us; 0us; 0x8000us; 0x8000us; 0x4000us; 0xC000us; 0x2000us ]
                let! data0 = Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 2us; 3us; 40us ]
                return { NewState = newState; Flags = flags; Data0 = data0; Data1 = 0us }
            }
        let! entries = Gen.arrayOfLength (nStates * nClasses) entryGen
        let! words = Gen.arrayOfLength 16 (Gen.elements [ 0; 1; 2; 3; 7; 40; 0xFFFF; 0x7FFF; 0x8000; 501; 0xFFFB ])
        let body = words |> Array.collect (fun word -> be16 (uint32 word))
        let! rangeEnd = Gen.elements [ body.Length; body.Length; body.Length; body.Length - 2; body.Length - 5; 8; 3; 0 ]
        let! actionsAt = Gen.elements [ 0L; 0L; 2L; 4L; 6L; 8L ]
        let! anchorLists =
            Gen.arrayOfLength 9 (gen {
                let! count = Gen.choose (0, 3)
                let! anchors = Gen.arrayOfLength count (Gen.zip (Gen.elements [ 0; 1; -5; 100; -200; 32767; -32768 ]) (Gen.elements [ 0; 1; -5; 100; -200; 32767; -32768 ]))
                let! present = Gen.elements [ true; true; false ]
                return present, anchors })
        let model = anchorLists |> Array.mapi (fun glyph (present, anchors) -> glyph, present, anchors) |> Array.filter (fun (_, present, _) -> present) |> Array.map (fun (glyph, _, anchors) -> uint32 glyph, anchors) |> Map.ofArray
        let! kind = Gen.elements [ Dense; Segments; SegmentArrays; Singles; Trimmed ]
        let! xScale = Gen.elements [ 1000; 2048; 500; 777 ]
        let! yScale = Gen.elements [ 1000; 640; 1500 ]
        let! count = Gen.choose (0, 12)
        let! codepoints = Gen.arrayOfLength count (Gen.choose (0, nClasses - 1) |> Gen.map uint32)
        let! steps = Gen.arrayOfLength count (Gen.elements [ 0u; 1u; 1u; 2u ])
        let clusters = Array.scan (+) 0u steps |> Array.take count
        let! attachTypes = Gen.arrayOfLength count (Gen.elements [ 0u; 1u; 2u; 2u ])
        let! chains = Gen.arrayOfLength count (Gen.elements [ -2; -1; 0; 1; 2; 3 ])
        let! offsets = Gen.arrayOfLength count (Gen.zip (Gen.choose (-30, 30)) (Gen.choose (-30, 30)))
        let! advances = Gen.arrayOfLength count (Gen.choose (-50, 50))
        let! horizontal = Gen.elements [ true; false ]
        let! reversed = Gen.elements [ true; false ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters ]
        let! maxOps = Gen.elements [ 1; 2; 3; 5; 8; 13; 40; 511; 512; 513; 514; 1000; 2000 ]
        let positions =
            Array.init count (fun i ->
                { XAdvance = advances[i]; YAdvance = 0; XOffset = fst offsets[i]; YOffset = snd offsets[i]; Var = attachVar attachTypes[i] chains[i] })
        let spec = { TableBytes = ReadOnlyMemory body; RangeEnd = rangeEnd; ActionsAt = actionsAt; ActionType = actionType }
        return
            { Literal =
                { Machine = classMachine nClasses entries
                  Codepoints = codepoints
                  Clusters = clusters
                  Masks = Array.create count 0u
                  Positions = positions
                  Level = level
                  MaxOps = maxOps
                  NumGlyphs = GlyphCount }
              Spec = spec
              Model = model
              Kind = kind
              XScale = xScale
              YScale = yScale
              Horizontal = horizontal
              Reversed = reversed }
    }

/// <summary>Runs the driver over an input and returns every field of the run the pass can change.</summary>
/// <param name="input">The input.</param>
/// <param name="font">The font.</param>
/// <returns>The masks, positions, budget, success flag and scratch flags.</returns>
let private driverFields (input: Input4) (font: Font) =
    let literal = input.Literal
    let count = literal.Codepoints.Length
    let run =
        { GlyphRun.empty with
            Infos = Sealed.init count (fun i -> { Codepoint = literal.Codepoints[i]; Mask = literal.Masks[i]; Cluster = literal.Clusters[i]; Var1 = 0u; Var2 = 0u })
            Props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
            Positions = Sealed.init count (fun i -> literal.Positions[i])
            HavePositions = true
            ClusterLevel = literal.Level
            MaxOps = literal.MaxOps }
    let result = KerxFormat4Apply.apply input.Spec literal.Machine font input.Horizontal input.Reversed run
    struct (result.Infos |> Seq.map (fun info -> info.Mask) |> Seq.toArray,
            result.Positions |> Seq.toArray,
            result.MaxOps,
            result.Successful,
            uint32 result.ScratchFlags)

/// <summary>Runs the literal transliteration and returns the same fields.</summary>
/// <param name="input">The input.</param>
/// <param name="font">The font.</param>
/// <returns>The masks, positions, budget, success flag and scratch flags.</returns>
let private literalFields (input: Input4) (font: Font) =
    let drive = LiteralDrive input.Literal
    drive.Drive ((fun (entry: Entry) -> entry.Data0 <> 0xFFFFus), literalTransition input font drive)
    struct (drive.Masks, drive.Positions, drive.MaxOps, drive.Successful, drive.Scratch)

/// <summary>The property: the driver and the literal agree on every field, for one action type.</summary>
/// <param name="name">The test name.</param>
/// <param name="actionType">The action type.</param>
/// <returns>The test.</returns>
let private literalProperty (name: string) (actionType: int) =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } name <|
        Prop.forAll (Arb.fromGen (inputGen actionType)) (fun input ->
            let table = match actionType with | 1 -> ankr input.Kind GlyphCount input.Model | _ -> Array.empty
            let font = fontWith table input.XScale input.YScale
            let struct (masks, positions, maxOps, successful, scratch) = driverFields input font
            let struct (literalMasks, literalPositions, literalMaxOps, literalSuccessful, literalScratch) = literalFields input font
            let show (positions: GlyphPosition array) = positions |> Array.map (fun p -> $"{p.XAdvance}/{p.YAdvance}/{p.XOffset}/{p.YOffset}/{p.Var}") |> String.concat " "
            let machineText = input.Literal.Machine.Entries |> Seq.map (fun e -> $"[{e.NewState},{e.Flags:X},{e.Data0:X}]") |> String.concat ""
            let context = $"type={actionType} end={input.Spec.RangeEnd} at={input.Spec.ActionsAt} horiz={input.Horizontal} reversed={input.Reversed} scale={input.XScale}/{input.YScale} kind={input.Kind} ops={input.Literal.MaxOps} text={String.Join(',', input.Literal.Codepoints)} clusters={String.Join(',', input.Literal.Clusters)} pos={show input.Literal.Positions} entries={machineText} body={Convert.ToHexString(input.Spec.TableBytes.ToArray())} model={input.Model}"
            Expect.equal (String.concat "," (Array.map string masks)) (String.concat "," (Array.map string literalMasks)) $"glyph flags {context}"
            Expect.equal (show positions) (show literalPositions) $"positions ours={show positions} literal={show literalPositions} {context}"
            Expect.equal maxOps literalMaxOps $"budget ours={maxOps} literal={literalMaxOps} {context}"
            Expect.equal successful literalSuccessful $"success flag {context}"
            Expect.equal scratch literalScratch $"scratch flags {context}"
            true)

/// <summary>Runs the pair of glyph-0 run and the mark-and-act machine over two glyphs and returns the run.</summary>
/// <param name="font">The font.</param>
/// <param name="spec">The spec.</param>
/// <param name="horizontal">Whether the run is horizontal.</param>
/// <param name="reversed">Whether the buffer is reversed.</param>
/// <param name="positions">The two positions.</param>
/// <returns>The run after the subtable.</returns>
let private applyTwo (font: Font) (spec: Format4Spec) (horizontal: bool) (reversed: bool) (positions: GlyphPosition array) =
    KerxFormat4Apply.apply spec markAndAct font horizontal reversed (format4Run [| 0u; 0u |] positions GlyphRun.MaxOpsDefault)

/// <summary>The ankr model of the hand-built cases: glyph 0 has two anchors.</summary>
let private handAnkr = ankr Dense GlyphCount (Map.ofList [ 0u, [| (100, 200); (30, 50) |] ])

/// <summary>The P8.S12 kerx format 4 unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S12" [
        test "an anchor point action subtracts the scaled ankr anchors and attaches to the mark" {
            let font = fontWith handAnkr 1000 1000
            let spec = specOf 1 (Array.concat [ be16 0u; be16 1u ])
            let result = applyTwo font spec true false [| plainPosition; plainPosition |]
            Expect.equal result.Positions[0] plainPosition "the marked glyph is not moved"
            Expect.equal result.Positions[1].XOffset 70 "mark x 100 minus current x 30"
            Expect.equal result.Positions[1].YOffset 150 "mark y 200 minus current y 50"
            Expect.equal result.Positions[1].XAdvance 100 "the advance is untouched"
            Expect.equal result.Positions[1].Var 0x0001FFFFu "mark attachment, chain to the glyph before"
            Expect.equal (result.ScratchFlags &&& ScratchFlags.HasGposAttachment) ScratchFlags.HasGposAttachment "the attachment flag"
            let reversed = applyTwo font spec true true [| plainPosition; plainPosition |]
            Expect.equal reversed.Positions[1].Var 0x00010001u "a reversed buffer negates the chain"
        }
        test "the anchors are scaled before they are subtracted" {
            let font = fontWith handAnkr 2000 500
            let spec = specOf 1 (Array.concat [ be16 0u; be16 1u ])
            let result = applyTwo font spec true false [| plainPosition; plainPosition |]
            Expect.equal result.Positions[1].XOffset (Font.emScaleX font 100 - Font.emScaleX font 30) "x scaled then subtracted"
            Expect.equal result.Positions[1].YOffset (Font.emScaleY font 200 - Font.emScaleY font 50) "y scaled then subtracted"
            Expect.equal result.Positions[1].XOffset 140 "the x scale doubles"
            Expect.equal result.Positions[1].YOffset 75 "the y scale halves"
        }
        test "a glyph or an anchor index the ankr table lacks is the null anchor" {
            let font = fontWith handAnkr 1000 1000
            let spec = specOf 1 (Array.concat [ be16 0u; be16 5u ])
            let result = applyTwo font spec true false [| plainPosition; plainPosition |]
            Expect.equal result.Positions[1].XOffset 100 "the missing current anchor is (0, 0)"
            Expect.equal result.Positions[1].YOffset 200 "the missing current anchor is (0, 0)"
            let noTable = fontWith Array.empty 1000 1000
            let bare = applyTwo noTable (specOf 1 (Array.concat [ be16 0u; be16 1u ])) true false [| plainPosition; plainPosition |]
            Expect.equal bare.Positions[1].XOffset 0 "no ankr table, no anchors"
            Expect.equal bare.Positions[1].Var 0x0001FFFFu "the attachment is recorded all the same"
        }
        test "a control point coordinate action subtracts four scaled coordinates" {
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 2 (Array.concat [ be16 10u; be16 20u; be16 3u; be16 (uint32 (uint16 -5s)) ])
            let result = applyTwo font spec true false [| plainPosition; plainPosition |]
            Expect.equal result.Positions[1].XOffset 7 "mark x 10 minus current x 3"
            Expect.equal result.Positions[1].YOffset 25 "mark y 20 minus current y -5"
        }
        test "the base offset is added to the cross-stream axis" {
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 2 (Array.concat [ be16 10u; be16 20u; be16 3u; be16 5u ])
            let marked = { plainPosition with XOffset = 9; YOffset = 4 }
            let horizontal = applyTwo font spec true false [| marked; plainPosition |]
            Expect.equal horizontal.Positions[1].YOffset 19 "y 15 plus the mark's y offset 4"
            Expect.equal horizontal.Positions[1].XOffset 7 "x is not shifted"
            let vertical = applyTwo font spec false false [| marked; plainPosition |]
            Expect.equal vertical.Positions[1].XOffset 16 "x 7 plus the mark's x offset 9"
            Expect.equal vertical.Positions[1].YOffset 15 "y is not shifted"
        }
        test "the cursive chain of the mark is resolved against the offset just written" {
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 2 (Array.concat [ be16 10u; be16 20u; be16 3u; be16 5u ])
            let marked = { plainPosition with YOffset = 10; Var = attachVar 2u 1 }
            let result = applyTwo font spec true false [| marked; plainPosition |]
            Expect.equal result.Positions[1].YOffset 40 "15 plus the mark's 10 plus the current glyph's own new 15, which the chain leads back to"
        }
        test "an action type no case handles moves nothing and still attaches" {
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 3 (Array.create 8 0uy)
            let moved = { plainPosition with XOffset = 5; YOffset = 6 }
            let result = applyTwo font spec true false [| plainPosition; moved |]
            Expect.equal result.Positions[1].XOffset 5 "the x offset stays"
            Expect.equal result.Positions[1].YOffset 6 "the y offset stays"
            Expect.equal result.Positions[1].Var 0x0001FFFFu "the attachment is recorded"
        }
        test "a control point action does nothing with the ot font functions, which have no contour points" {
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 0 (Array.create 8 0uy)
            let result = applyTwo font spec true false [| plainPosition; plainPosition |]
            Expect.equal result.Positions[1] plainPosition "nothing moved"
            Expect.equal (result.ScratchFlags &&& ScratchFlags.HasGposAttachment) ScratchFlags.Default "no attachment"
            Expect.equal result.MaxOps (GlyphRun.MaxOpsDefault - 512 - 2) "the two outline lookups are charged, and the break marking of the pair"
        }
        test "an action that attaches moves the mark to the glyph it attached, a control point action never attaches" {
            let font = fontWith Array.empty 1000 1000
            let machine = entryMachine { NewState = 0; Flags = 0x8000us; Data0 = 0us; Data1 = 0us }
            let coordinates = specOf 2 (Array.concat [ be16 10u; be16 20u; be16 3u; be16 5u ])
            let attached = KerxFormat4Apply.apply coordinates machine font true false (format4Run [| 0u; 0u; 0u |] [| plainPosition; plainPosition; plainPosition |] GlyphRun.MaxOpsDefault)
            Expect.equal attached.Positions[2].Var 0x0001FFFFu "after a coordinate action the mark moves to the glyph before: chain -1"
            let controlPoints = specOf 0 (Array.create 8 0uy)
            let untouched = KerxFormat4Apply.apply controlPoints machine font true false (format4Run [| 0u; 0u; 0u |] [| plainPosition; plainPosition; plainPosition |] GlyphRun.MaxOpsDefault)
            Expect.equal untouched.Positions[2] plainPosition "a control point action attaches nothing, however often it runs"
        }
        test "an endless DontAdvance machine ends when the budget is spent" {
            let machine = entryMachine { NewState = 0; Flags = 0x4000us; Data0 = 0xFFFFus; Data1 = 0us }
            let spec = specOf 1 Array.empty
            let font = fontWith Array.empty 1000 1000
            let result = KerxFormat4Apply.apply spec machine font true false (format4Run [| 0u |] [| plainPosition |] 50)
            Expect.equal result.MaxOps (-1) "the budget is spent by the epsilon transitions, the failing check charging too"
            Expect.equal result.Positions[0] plainPosition "nothing was moved"
        }
        test "the ankr table loads from a face and answers anchors" {
            let font = fontWith (ankr Singles GlyphCount (Map.ofList [ 3u, [| (7, -8) |]; 9u, [||] ])) 1000 1000
            let table = font.Ot.Ankr.Value
            Expect.equal (Ankr.anchor table 3u 0u GlyphCount) (struct (7, -8)) "glyph 3 has one anchor"
            Expect.equal (Ankr.anchor table 3u 1u GlyphCount) (struct (0, 0)) "its second anchor is the null anchor"
            Expect.equal (Ankr.anchor table 9u 0u GlyphCount) (struct (0, 0)) "glyph 9 has an empty array"
            Expect.equal (Ankr.anchor table 4u 0u GlyphCount) (struct (0, 0)) "glyph 4 is not in the lookup"
        }
        test "an ankr table of version one is the null table" {
            let bytes = ankr Dense GlyphCount (Map.ofList [ 0u, [| (1, 2) |] ])
            let changed = Array.copy bytes
            changed[1] <- 1uy
            let font = fontWith changed 1000 1000
            Expect.equal (Ankr.anchor font.Ot.Ankr.Value 0u 0u GlyphCount) (struct (0, 0)) "the table is rejected"
        }
        literalProperty "the format 4 driver equals a literal transliteration with control point actions" 0
        literalProperty "the format 4 driver equals a literal transliteration with anchor point actions" 1
        literalProperty "the format 4 driver equals a literal transliteration with control point coordinate actions" 2
        literalProperty "the format 4 driver equals a literal transliteration with an unknown action type" 3
        test "a skipped kerx format 4 subtable allocates nothing" {
            let table = kerx4 [| 1u; 2u; 3u |] 2 [| 10; 20; 3; 5; 0; 0; 0; 0 |] false
            let font = kerxFont table.Bytes
            let plan = kerxPlan ()
            let run = format4Run [| 9u; 9u; 9u |] [| plainPosition; plainPosition; plainPosition |] GlyphRun.MaxOpsDefault
            AatLayout.position plan font run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> AatLayout.position plan font run |> ignore)) 0L "a subtable none of the glyphs can start allocates nothing"
        }
        test "a format 4 pass that changes nothing allocates nothing" {
            let table = kerx4 [| 1u; 2u; 3u |] 2 [| 10; 20; 3; 5; 0; 0; 0; 0 |] false
            let font = kerxFont table.Bytes
            let kerx = font.Ot.Kerx.Value
            let { Body = body; RangeEnd = rangeEnd; KernActionAt = actionsAt; Machine = machineOption } = kerx.Subtables[0]
            let spec = { TableBytes = body; RangeEnd = rangeEnd; ActionsAt = int64 actionsAt; ActionType = 2 }
            let machine = machineOption.Value
            let run = format4Run [| 1u; 1u; 1u |] [| plainPosition; plainPosition; plainPosition |] GlyphRun.MaxOpsDefault
            let result = KerxFormat4Apply.apply spec machine font true false run
            Expect.equal result.Positions[2] plainPosition "the first glyph's class only marks, so nothing moves"
            KerxFormat4Apply.apply spec machine font true false run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> KerxFormat4Apply.apply spec machine font true false run |> ignore)) 0L "marking alone allocates nothing"
        }
        test "a non-actionable format 4 transition allocates nothing" {
            let machine = entryMachine nullEntry
            let font = fontWith Array.empty 1000 1000
            let spec = specOf 2 Array.empty
            let run = format4Run [| 0u |] [| plainPosition |] GlyphRun.MaxOpsDefault
            KerxFormat4Apply.apply spec machine font true false run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> KerxFormat4Apply.apply spec machine font true false run |> ignore)) 0L "a null transition batches without allocating"
        }
        test "a kerx format 4 pass reads the table, attaches the glyphs and leaves the run attached" {
            let table = kerx4 [| 1u; 2u; 3u |] 2 [| 10; 20; 3; 5; 7; 1; 2; 9 |] false
            let font = kerxFont table.Bytes
            let plan = kerxPlan ()
            let run = format4Run [| 1u; 2u; 3u |] [| plainPosition; plainPosition; plainPosition |] GlyphRun.MaxOpsDefault
            let result = AatLayout.position plan font run
            Expect.equal result.Positions[1].XOffset 7 "glyph 2 runs action 0: 10 minus 3"
            Expect.equal result.Positions[1].YOffset 15 "glyph 2 runs action 0: 20 minus 5"
            Expect.equal result.Positions[1].Var 0x0001FFFFu "attached to glyph 1"
            Expect.equal result.Positions[2].XOffset 5 "glyph 3 runs action 1: 7 minus 2"
            Expect.equal result.Positions[2].YOffset (-8) "glyph 3 runs action 1: 1 minus 9"
            Expect.equal result.Positions[2].Var 0x0001FFFEu "attached to the mark, glyph 1, two glyphs before, since action 0 did not move the mark"
        }
    ]
