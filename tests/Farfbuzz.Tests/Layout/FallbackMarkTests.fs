module Farfbuzz.Tests.Layout.FallbackMarkTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a valid 54-byte head table.</summary>
/// <param name="upem">Units per em.</param>
/// <returns>The bytes.</returns>
let private head (upem: int) : byte array =
    Array.concat
        [ be32 0x00010000u
          be32 0x00010000u
          be32 0u
          be32 0x5F0F3CF5u
          be16 0u
          be16 (uint32 upem)
          be32 0u; be32 0u
          be32 0u; be32 0u
          be16 0u; be16 0u
          be16 0u; be16 0u
          be16 0u
          be16 0u
          be16 0u
          be16 0u
          be16 0u ]

/// <summary>Builds a version 0.5 maxp table.</summary>
/// <param name="glyphCount">The number of glyphs.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00005000u; be16 (uint32 glyphCount) ]

/// <summary>Builds one 10-byte glyph header.</summary>
/// <param name="xMin">The minimum x.</param>
/// <param name="yMin">The minimum y.</param>
/// <param name="xMax">The maximum x.</param>
/// <param name="yMax">The maximum y.</param>
/// <returns>The bytes.</returns>
let private glyphHeader (xMin: int) (yMin: int) (xMax: int) (yMax: int) : byte array =
    Array.concat
        [ be16 1u
          be16 (uint32 (xMin &&& 0xFFFF))
          be16 (uint32 (yMin &&& 0xFFFF))
          be16 (uint32 (xMax &&& 0xFFFF))
          be16 (uint32 (yMax &&& 0xFFFF)) ]

/// <summary>Builds a short-format loca table.</summary>
/// <param name="offsets">The byte offsets.</param>
/// <returns>The bytes.</returns>
let private shortLoca (offsets: int array) : byte array =
    offsets |> Array.collect (fun offset -> be16 (uint32 (offset / 2)))

/// <summary>Builds a 36-byte hea table.</summary>
/// <param name="longMetricCount">The long-metric count.</param>
/// <returns>The bytes.</returns>
let private hea (longMetricCount: int) : byte array =
    Array.concat
        [ be32 0x00010000u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u
          be16 (uint32 longMetricCount) ]

/// <summary>Builds an hmtx table from advance and bearing pairs.</summary>
/// <param name="metrics">The pairs.</param>
/// <returns>The bytes.</returns>
let private hmtx (metrics: (int * int) array) : byte array =
    metrics |> Array.collect (fun (advance, bearing) -> Array.concat [ be16 (uint32 advance); be16 (uint32 (bearing &&& 0xFFFF)) ])

/// <summary>Builds a three-glyph glyf face: glyph 0 empty, glyph 1 with the given box, glyph 2 with bounds
/// (0, 0, 20, 20).</summary>
/// <param name="advances">The hmtx advances and bearings, three of them.</param>
/// <param name="baseBox">The base glyph's (xMin, yMin, xMax, yMax).</param>
/// <returns>The face.</returns>
let private glyfFace (advances: (int * int) array) (baseBox: int * int * int * int) : Face =
    let xMin, yMin, xMax, yMax = baseBox
    let glyf = Array.concat [ glyphHeader 0 0 0 0; glyphHeader xMin yMin xMax yMax; glyphHeader 0 0 20 20 ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000
                  Tags.Maxp, maxp 3
                  Tags.Loca, shortLoca [| 0; 10; 20; 30 |]
                  Tags.Glyf, glyf
                  Tags.Hhea, hea 3
                  Tags.Hmtx, hmtx advances ])) 0u

/// <summary>The face of <see cref="glyfFace"/> with a VARC table, whose outlines the extents do not read yet.</summary>
/// <returns>The face.</returns>
let private varcFace () : Face =
    let glyf = Array.concat [ glyphHeader 0 0 0 0; glyphHeader 0 0 100 100; glyphHeader 0 0 20 20 ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000
                  Tags.Maxp, maxp 3
                  Tags.Loca, shortLoca [| 0; 10; 20; 30 |]
                  Tags.Glyf, glyf
                  Tags.Varc, Array.zeroCreate 14
                  Tags.Hhea, hea 3
                  Tags.Hmtx, hmtx [| 0, 0; 100, 0; 20, 0 |] ])) 0u

/// <summary>The default font: base glyph 1 with bounds (0, 0, 100, 100) and advance 100.</summary>
/// <returns>The font.</returns>
let private font () : Font =
    Font.create (glyfFace [| 0, 0; 100, 0; 20, 0 |] (0, 0, 100, 100))

/// <summary>The packed Unicode properties of a code point.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The properties.</returns>
let private unicodeProps (codepoint: uint32) : uint16 =
    let struct (props, _) = UnicodeProps.compute codepoint
    props

/// <summary>The properties of a code point, base or mark.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The properties.</returns>
let private propsOf (codepoint: uint32) : GlyphProps =
    { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicodeProps codepoint }

/// <summary>Builds a positioned run.</summary>
/// <param name="direction">The direction.</param>
/// <param name="items">The glyph id and properties of each glyph.</param>
/// <param name="advances">The advance of each glyph.</param>
/// <returns>The run.</returns>
let private run (direction: Direction) (items: (uint32 * GlyphProps) array) (advances: (int * int) array) : GlyphRun =
    let infos = items |> Array.mapi (fun index (glyph, _) -> { Codepoint = glyph; Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = items |> Array.map snd
    let positions = advances |> Array.map (fun (x, y) -> { XAdvance = x; YAdvance = y; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = ImmutableArray.CreateRange infos
        Props = ImmutableArray.CreateRange props
        Positions = ImmutableArray.CreateRange positions
        HavePositions = true
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.NoLanguage } }

/// <summary>The plan of a fallback-mark positioner over the default font.</summary>
/// <param name="direction">The direction.</param>
/// <returns>The plan.</returns>
let private plan (direction: Direction) : OtShapePlan =
    { Props = { Direction = direction; Script = Scripts.Latin; Language = Language.NoLanguage }
      Shaper = { Kind = DefaultShaper; GposTag = Tags.Empty; Normalization = NormalizationNone; ZeroWidthMarks = ZeroWidthMarksNone; FallbackPosition = true }
      Map = OtMap.empty
      Data = NoShaperData
      UserFeatures = ImmutableArray.Empty
      FracMask = 0u; NumrMask = 0u; DnomMask = 0u; RtlmMask = 0u; KernMask = 0u
      RequestedKerning = false; HasFrac = false; HasVert = false; HasGposMark = false; ZeroMarks = false
      FallbackGlyphClasses = true; FallbackMarkPositioning = true; AdjustMarkPositioningWhenZeroing = false
      ApplyGpos = false; ApplyKern = false; ApplyFallbackKern = false; ApplyKerx = false; ApplyMorx = false; ApplyTrak = false; Unicode = ValueNone }

/// <summary>Positions one run through the fallback mark positioner.</summary>
/// <param name="direction">The direction.</param>
/// <param name="items">The glyphs.</param>
/// <param name="advances">The advances.</param>
/// <returns>The positioned run.</returns>
let private positioned (direction: Direction) (items: (uint32 * GlyphProps) array) (advances: (int * int) array) : GlyphRun =
    match FallbackMark.position (plan direction) (font ()) false (run direction items advances) with
    | Ok positioned -> positioned
    | Error error -> failtest $"unexpected error {error}"

/// <summary>The base glyph of a run.</summary>
let private baseGlyph = 1u

/// <summary>The mark glyph of a run.</summary>
let private markGlyph = 2u

/// <summary>The combining dot below, U+0323, class 220.</summary>
[<Literal>]
let private BelowDot = 0x0323u

/// <summary>The combining acute, U+0301, class 230.</summary>
[<Literal>]
let private Acute = 0x0301u

/// <summary>The P5.S14 fallback-mark tests.</summary>
[<Tests>]
let tests =
    testList "P5.S14" [
        test "Hebrew and Thai marks recategorize" {
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x05B0u (uint32 (Unicode.modifiedCombiningClass 0x05B0u))) 220u "sheva becomes below"
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x05C1u (uint32 (Unicode.modifiedCombiningClass 0x05C1u))) 232u "shin dot becomes above-right"
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x05C2u (uint32 (Unicode.modifiedCombiningClass 0x05C2u))) 228u "sin dot becomes above-left"
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x05BFu (uint32 (Unicode.modifiedCombiningClass 0x05BFu))) 214u "rafe becomes attached-above"
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x0E31u (uint32 (Unicode.modifiedCombiningClass 0x0E31u))) 232u "Thai mai han-akat becomes above-right"
            Expect.equal (FallbackMark.recategorizeCombiningClass 0x0300u 230u) 230u "an already-recategorized class is unchanged"
        }
        test "a below mark is shifted down by the gap and an above mark up" {
            let below = positioned Direction.Ltr [| baseGlyph, propsOf 0x0061u; markGlyph, propsOf BelowDot |] [| 100, 0; 20, 0 |]
            Expect.equal (below.Positions[1].XOffset, below.Positions[1].YOffset, below.Positions[1].XAdvance, below.Positions[1].YAdvance) (-60, -82, 0, 0) "below mark"
            let above = positioned Direction.Ltr [| baseGlyph, propsOf 0x0061u; markGlyph, propsOf Acute |] [| 100, 0; 20, 0 |]
            Expect.equal (above.Positions[1].XOffset, above.Positions[1].YOffset, above.Positions[1].XAdvance, above.Positions[1].YAdvance) (-60, 162, 0, 0) "above mark"
        }
        test "a below mark is never shifted up" {
            // A flat base at y=100 (height 0) and a mark whose top sits at y=20: the raw below offset is +18 (up),
            // so the never-shift-up rule pins it to zero.
            let flatFont = Font.create (glyfFace [| 0, 0; 100, 0; 20, 0 |] (0, 100, 100, 100))
            match FallbackMark.position (plan Direction.Ltr) flatFont false (run Direction.Ltr [| baseGlyph, propsOf 0x0061u; markGlyph, propsOf BelowDot |] [| 100, 0; 20, 0 |]) with
            | Ok positioned -> Expect.equal positioned.Positions[1].YOffset 0 "the below mark is pinned to zero"
            | Error error -> failtest $"unexpected error {error}"
        }
        test "marks split across a three-component ligature in LTR and RTL" {
            let ligId = 1uy
            let ligFont = Font.create (glyfFace [| 0, 0; 300, 0; 20, 0 |] (0, 0, 100, 100))
            let baseGlyphProps =
                { (GlyphPropsBits.setLigPropsForLigature ligId 3uy (Unchecked.defaultof<GlyphProps>)) with
                    UnicodeProps = unicodeProps 0x0061u
                    GlyphClass = GlyphPropsBits.Ligature }
            let ligMark (componentIndex: byte) : GlyphProps =
                { (GlyphPropsBits.setLigPropsForMark ligId componentIndex (Unchecked.defaultof<GlyphProps>)) with
                    UnicodeProps = unicodeProps Acute
                    GlyphClass = 0us }
            let items =
                [| baseGlyph, baseGlyphProps
                   markGlyph, ligMark 1uy
                   markGlyph, ligMark 2uy
                   markGlyph, ligMark 3uy |]
            let advances = [| 300, 0; 20, 0; 20, 0; 20, 0 |]
            let positionedWith (direction: Direction) : GlyphRun =
                match FallbackMark.position (plan direction) ligFont false (run direction items advances) with
                | Ok positioned -> positioned
                | Error error -> failtest $"unexpected error {error}"
            let ltr = positionedWith Direction.Ltr
            Expect.equal (ltr.Positions[1].XOffset, ltr.Positions[2].XOffset, ltr.Positions[3].XOffset) (-260, -160, -60) "LTR component boxes left to right"
            let rtl = positionedWith Direction.Rtl
            Expect.equal (rtl.Positions[1].XOffset, rtl.Positions[2].XOffset, rtl.Positions[3].XOffset) (240, 140, 40) "RTL component boxes right to left"
        }
        test "a default-ignorable first glyph starts a cluster: the default-ignorable glyphs after it are unsafe to break, none when a mark comes first" {
            let flagged (items: (uint32 * GlyphProps) array) : uint32 array =
                match OtLayout.fallbackMarkPosition (plan Direction.Ltr) (font ()) false (run Direction.Ltr items (items |> Array.map (fun _ -> 0, 0))) with
                | Ok positioned -> [| for info in positioned.Infos -> info.Mask &&& uint32 GlyphFlags.UnsafeToBreak |]
                | Error error -> failtest $"unexpected error {error}"
            let zwnj = propsOf 0x200Cu
            Expect.equal (flagged [| baseGlyph, zwnj; baseGlyph, zwnj |]) [| 0u; 1u |] "two zero width non-joiners: the second is unsafe to break"
            Expect.equal (flagged [| baseGlyph, zwnj; baseGlyph, zwnj; baseGlyph, zwnj |]) [| 0u; 1u; 1u |] "three: the cluster is the whole run"
            Expect.equal (flagged [| baseGlyph, zwnj; markGlyph, propsOf Acute |]) [| 0u; 1u |] "a mark after a default-ignorable first glyph"
            Expect.equal (flagged [| markGlyph, propsOf Acute; baseGlyph, zwnj |]) [| 0u; 0u |] "a mark first is no base: the default-ignorable glyph after it is a base of its own"
            Expect.equal (flagged [||]) [||] "an empty run"
        }
        test "a default-ignorable glyph after a mark is a base: the glyphs after it are unsafe to break" {
            let flagged (items: (uint32 * GlyphProps) array) : uint32 array =
                match OtLayout.fallbackMarkPosition (plan Direction.Ltr) (font ()) false (run Direction.Ltr items (items |> Array.map (fun _ -> 0, 0))) with
                | Ok positioned -> [| for info in positioned.Infos -> info.Mask &&& uint32 GlyphFlags.UnsafeToBreak |]
                | Error error -> failtest $"unexpected error {error}"
            let zwnj = propsOf 0x200Cu
            Expect.equal (flagged [| markGlyph, propsOf BelowDot; baseGlyph, zwnj; baseGlyph, zwnj; markGlyph, propsOf Acute |]) [| 0u; 0u; 1u; 1u |] "a mark, two joiners and a mark: the second joiner and the last mark"
            Expect.equal (flagged [| markGlyph, propsOf BelowDot; baseGlyph, zwnj; baseGlyph, zwnj |]) [| 0u; 0u; 1u |] "a mark and two joiners"
        }
        test "a base without extents zeroes the marks of a combining class of zero too, shifting them back when the offsets are adjusted" {
            let bindi = propsOf 0x0A02u
            let withAdjust (adjust: bool) : GlyphRun =
                match FallbackMark.position (plan Direction.Ltr) (font ()) adjust (run Direction.Ltr [| 9u, propsOf 0x004Eu; markGlyph, bindi |] [| 100, 0; 20, 0 |]) with
                | Ok positioned -> positioned
                | Error error -> failtest $"unexpected error {error}"
            let plain = withAdjust false
            Expect.equal (plain.Positions[1].XAdvance, plain.Positions[1].XOffset) (0, 0) "advance zeroed, offset untouched"
            let adjusted = withAdjust true
            Expect.equal (adjusted.Positions[1].XAdvance, adjusted.Positions[1].XOffset) (0, -20) "advance zeroed, offset shifted back by the advance"
        }
        test "a base with extents leaves a mark of a combining class of zero alone" {
            let bindi = propsOf 0x0A02u
            match FallbackMark.position (plan Direction.Ltr) (font ()) true (run Direction.Ltr [| baseGlyph, propsOf 0x004Eu; markGlyph, bindi |] [| 100, 0; 20, 0 |]) with
            | Ok positioned -> Expect.equal (positioned.Positions[1].XAdvance, positioned.Positions[1].XOffset) (20, 0) "no advance zeroed, no offset"
            | Error error -> failtest $"unexpected error {error}"
        }
        test "a face whose outlines are not ported reports it for a non-spacing mark of combining class zero, which is zeroed when the extents answer none, and not for a spacing mark" {
            let varc = Font.create (varcFace ())
            let positionedOn (mark: uint32) =
                FallbackMark.position (plan Direction.Ltr) varc false (run Direction.Ltr [| baseGlyph, propsOf 0x004Eu; markGlyph, propsOf mark |] [| 100, 0; 20, 0 |])
            Expect.equal (positionedOn 0x0A02u |> Result.isError) true "the bindi, class zero and non-spacing"
            Expect.equal (positionedOn 0x0903u |> Result.isOk) true "the visarga, class zero and spacing, is never zeroed"
        }
        test "a base without extents zeroes the mark advances" {
            // Glyph id 9 is beyond the three-glyph face, so Extents.glyph answers none and the mark advance is zeroed.
            let positioned = positioned Direction.Ltr [| 9u, propsOf 0x0061u; markGlyph, propsOf Acute |] [| 100, 0; 20, 0 |]
            Expect.equal (positioned.Positions[1].XAdvance, positioned.Positions[1].YAdvance) (0, 0) "mark advances zeroed"
            Expect.equal (positioned.Positions[1].XOffset, positioned.Positions[1].YOffset) (0, 0) "mark offsets untouched"
        }
    ]

/// <summary>The glyphs the order property draws: a base, a below mark, an above mark, the joiners and the word joiner (default
/// ignorable and no mark), the soft hyphen, and a hidden base.</summary>
let private orderGlyphs : (uint32 * GlyphProps) array =
    [| baseGlyph, propsOf 0x0061u
       markGlyph, propsOf BelowDot
       markGlyph, propsOf Acute
       baseGlyph, propsOf 0x200Cu
       baseGlyph, propsOf 0x200Du
       baseGlyph, propsOf 0x2060u
       baseGlyph, propsOf 0x00ADu
       baseGlyph, { propsOf 0x0061u with UnicodeProps = propsOf 0x0061u |> fun props -> props.UnicodeProps ||| UnicodeProps.Hidden } |]

/// <summary>Whether two runs agree on the glyphs, their properties and their positions.</summary>
/// <param name="a">The first run.</param>
/// <param name="b">The second run.</param>
/// <returns>True when they agree.</returns>
let private sameRun (a: GlyphRun) (b: GlyphRun) : bool =
    a.Infos.AsSpan().SequenceEqual(b.Infos.AsSpan()) && a.Props.AsSpan().SequenceEqual(b.Props.AsSpan()) && a.Positions.AsSpan().SequenceEqual(b.Positions.AsSpan())

/// <summary>The cases of the fallback mark positioning's early exit: every order of bases, marks and glyphs that are
/// neither, in both directions, against the pass that has no early exit.</summary>
[<Tests>]
let orderTests =
    testList "fallback mark order" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 20000 } "fallbackMarkPosition equals the pass on every order of bases, marks, joiners and hidden glyphs" (
            Prop.forAll (Arb.fromGen (gen {
                let! length = Gen.choose (0, 8)
                let! picks = Gen.listOfLength length (Gen.choose (0, orderGlyphs.Length - 1))
                let! rtl = Gen.elements [ false; true ]
                return picks, rtl })) (fun (picks, rtl) ->
                let direction = match rtl with | true -> Direction.Rtl | false -> Direction.Ltr
                let items = picks |> List.map (fun pick -> orderGlyphs[pick]) |> List.toArray
                let input = run direction items (items |> Array.map (fun _ -> 100, 0))
                match OtLayout.fallbackMarkPosition (plan direction) (font ()) false input, OtLayout.fallbackMarkPositionPass (plan direction) (font ()) false input with
                | Ok actual, Ok expected -> sameRun actual expected
                | _ -> false))

        test "a default-ignorable glyph between two marks is the base of the second" {
            let flagged (items: (uint32 * GlyphProps) array) : uint32 array =
                match OtLayout.fallbackMarkPosition (plan Direction.Ltr) (font ()) false (run Direction.Ltr items (items |> Array.map (fun _ -> 0, 0))) with
                | Ok positioned -> [| for info in positioned.Infos -> info.Mask &&& uint32 GlyphFlags.UnsafeToBreak |]
                | Error error -> failtest $"unexpected error {error}"
            let zwnj = propsOf 0x200Cu
            Expect.equal (flagged [| markGlyph, propsOf Acute; baseGlyph, zwnj; markGlyph, propsOf Acute |]) [| 0u; 0u; 1u |] "mark, joiner, mark: the joiner is the base of the last mark"
            Expect.equal (flagged [| markGlyph, propsOf Acute; baseGlyph, zwnj; baseGlyph, zwnj |]) [| 0u; 0u; 1u |] "mark, joiner, joiner: the second joiner trails the first"
        }
    ]
