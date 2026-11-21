module Farfbuzz.Tests.Layout.GsubSimpleTests

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>An empty GDEF for the apply tests.</summary>
let private emptyGdef =
    { Version = 0u
      GlyphClasses = ClassDefNull
      MarkAttachClasses = ClassDefNull
      MarkGlyphSets = ImmutableArray.Empty
      LigCarets = ReadOnlyMemory.Empty
      AttachList = ReadOnlyMemory.Empty
      HasVarStore = false
      HasGlyphClasses = false
      LigCaretsWide = false
      GlyphProps = ImmutableArray.Empty }

/// <summary>Builds a glyph info with a code point and mask.</summary>
/// <param name="codepoint">The code point.</param>
/// <param name="mask">The mask.</param>
/// <returns>The glyph info.</returns>
let private infoOf (codepoint: uint32) (mask: uint32) =
    { Codepoint = codepoint; Mask = mask; Cluster = 0u; Var1 = 0u; Var2 = 0u }

/// <summary>Builds empty glyph properties.</summary>
/// <returns>The properties.</returns>
let private propsOf () =
    { UnicodeProps = 0us
      GlyphClass = 0us
      LigProps = 0uy
      Syllable = 0uy
      ShaperCategory = 0uy
      ShaperAuxiliary = 0uy
      GlyphIndex = 0u }

/// <summary>Builds a single-glyph run cursor for the subtable-apply tests.</summary>
/// <param name="info">The glyph.</param>
/// <param name="props">The glyph properties.</param>
/// <param name="randomState">The random state.</param>
/// <returns>The cursor.</returns>
let private cursorOf (info: GlyphInfo) (props: GlyphProps) (randomState: uint32) =
    let infos = [| info |]
    let propsArray = [| props |]
    RunCursor(
        ReadOnlySpan infos, ReadOnlySpan propsArray, 0,
        ReadOnlySpan.Empty, ReadOnlySpan.Empty, ReadOnlySpan.Empty,
        false, ClusterLevel.MonotoneGraphemes, BufferFlags.Default,
        0uy, randomState, 1000, true)

/// <summary>A glyph array of one width built from a list.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The immutable array.</returns>
let private glyphs (glyphs: uint32 list) =
    Sealed.ofList glyphs

/// <summary>Builds a single-coverage subtable covering one glyph.</summary>
/// <param name="glyph">The covered glyph.</param>
/// <returns>The coverage.</returns>
let private singleCoverage (glyph: uint32) =
    CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| glyph |]))

/// <summary>Builds a font and face from an upstream file.</summary>
/// <param name="path">The upstream font path.</param>
/// <returns>The font.</returns>
let private fontOf (path: string) =
    Font.create (Face.create (read path) 0u)

/// <summary>Builds a synthetic GSUB table of one single-substitution lookup over a coverage.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="delta">The delta.</param>
/// <returns>The table.</returns>
let private singleTableOf (coverage: Coverage) (delta: uint32) =
    let subtable = SingleDelta (coverage, delta, 0xFFFFu)
    let digest = Coverage.digest coverage
    let accel = { Subtable = subtable; SubtableDigest = digest }
    let lookup =
        { LookupType = 1
          Props = 0u
          IsReverse = false
          Subtables = ImmutableArray.Create accel
          LookupDigest = digest
          SecondDigest = Digest.full }
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy lookup)
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>A lookup map with a non-zero mask.</summary>
/// <returns>The lookup map.</returns>
let private lookupMapOf () =
    { LookupIndex = 0
      AutoZwnj = false
      AutoZwj = false
      Random = false
      PerSyllable = false
      Mask = 1u
      FeatureTag = Tag.make 'c' 'c' 'm' 'p' }

/// <summary>One stage holding the single lookup.</summary>
/// <returns>The stages.</returns>
let private stageOf () = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }

/// <summary>Builds a run of glyphs with a non-zero mask.</summary>
/// <param name="count">The glyph count.</param>
/// <returns>The run.</returns>
let private maskedRunOf (count: int) =
    let infos = Sealed.init count (fun index -> { Codepoint = uint32 (index % 128); Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>Measures the bytes one call allocates, after a warm-up call.</summary>
/// <param name="action">The call to measure.</param>
/// <returns>The bytes allocated by the measured call.</returns>
let private measureAlloc (action: unit -> unit) : int64 =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    action ()
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>The P5.S07 GSUB single, multiple and alternate cases.</summary>
[<Tests>]
let tests =
    testList "P5.S07" [
        test "test_ot_layout_lookup_get_glyph_alternates" {
            let font = fontOf "test/api/fonts/SourceSansPro-Regular.otf"
            let struct (total, alternates) = LayoutApi.glyphAlternates font.Ot 1 1091u 2 3
            Expect.equal total 7 "seven alternates"
            Expect.equal (alternates.AsSpan().ToArray()) [| 1606u; 1578u; 1592u |] "the alternates from offset 2"
        }
        test "test_ot_layout_lookup_collect_glyph_alternates single-format2" {
            let font = fontOf "test/api/fonts/SourceSansPro-Regular.otf"
            let collected = LayoutApi.collectGlyphAlternates font.Ot 0
            let map = Dictionary<uint32, ImmutableArray<uint32>>()
            for struct (glyph, alts) in collected do
                map[glyph] <- alts
            let mutable covered = 0
            for gid in 0u .. uint32 (font.Face.GlyphCount - 1) do
                let struct (total, alts) = LayoutApi.glyphAlternates font.Ot 0 gid 0 1000
                match total with
                | 0 -> Expect.isFalse (map.ContainsKey gid) $"glyph {gid} has no alternates"
                | _ ->
                    Expect.isTrue (map.ContainsKey gid) $"glyph {gid} is collected"
                    Expect.equal alts map[gid] "the collected alternates match the per-glyph getter"
                    covered <- covered + 1
            Expect.equal covered collected.Length "every collected glyph is covered"
        }
        test "test_ot_layout_lookup_collect_glyph_alternates alternate" {
            let font = fontOf "test/api/fonts/SourceSansPro-Regular.otf"
            let collected = LayoutApi.collectGlyphAlternates font.Ot 1
            let map = Dictionary<uint32, ImmutableArray<uint32>>()
            for struct (glyph, alts) in collected do
                map[glyph] <- alts
            let mutable covered = 0
            for gid in 0u .. uint32 (font.Face.GlyphCount - 1) do
                let struct (total, alts) = LayoutApi.glyphAlternates font.Ot 1 gid 0 1000
                match total with
                | 0 -> Expect.isFalse (map.ContainsKey gid) $"glyph {gid} has no alternates"
                | _ ->
                    Expect.isTrue (map.ContainsKey gid) $"glyph {gid} is collected"
                    Expect.equal alts map[gid] "the collected alternates match the per-glyph getter"
                    covered <- covered + 1
            Expect.equal covered collected.Length "every collected glyph is covered"
        }
        test "test_ot_layout_lookup_collect_glyph_alternates single-format1" {
            let font = fontOf "test/api/fonts/SourceSansPro-Regular.otf"
            let collected = LayoutApi.collectGlyphAlternates font.Ot 2
            let map = Dictionary<uint32, ImmutableArray<uint32>>()
            for struct (glyph, alts) in collected do
                map[glyph] <- alts
            let mutable covered = 0
            for gid in 0u .. uint32 (font.Face.GlyphCount - 1) do
                let struct (total, alts) = LayoutApi.glyphAlternates font.Ot 2 gid 0 1000
                match total with
                | 0 -> Expect.isFalse (map.ContainsKey gid) $"glyph {gid} has no alternates"
                | _ ->
                    Expect.isTrue (map.ContainsKey gid) $"glyph {gid} is collected"
                    Expect.equal alts map[gid] "the collected alternates match the per-glyph getter"
                    covered <- covered + 1
            Expect.equal covered collected.Length "every collected glyph is covered"
        }
        test "test_ot_layout_lookup_collect_glyph_alternates single-format1-many" {
            let font = fontOf "test/api/fonts/SourceSansPro-Regular.otf"
            let collected = LayoutApi.collectGlyphAlternates font.Ot 18
            let map = Dictionary<uint32, ImmutableArray<uint32>>()
            for struct (glyph, alts) in collected do
                map[glyph] <- alts
            let mutable covered = 0
            for gid in 0u .. uint32 (font.Face.GlyphCount - 1) do
                let struct (total, alts) = LayoutApi.glyphAlternates font.Ot 18 gid 0 1000
                match total with
                | 0 -> Expect.isFalse (map.ContainsKey gid) $"glyph {gid} has no alternates"
                | _ ->
                    Expect.isTrue (map.ContainsKey gid) $"glyph {gid} is collected"
                    Expect.equal alts map[gid] "the collected alternates match the per-glyph getter"
                    covered <- covered + 1
            Expect.equal covered collected.Length "every collected glyph is covered"
        }
        test "test_shape_plan_execute_buffer_limits" {
            let factor = 400
            let lookupLen = 12 + factor
            let words = ResizeArray<uint16>()
            words.Add 1us; words.Add 0us; words.Add 10us; words.Add 30us; words.Add 46us
            words.Add 1us; words.Add 0x4446us; words.Add 0x4C54us; words.Add 8us; words.Add 4us
            words.Add 0us; words.Add 0us; words.Add 0xFFFFus; words.Add 1us; words.Add 0us
            words.Add 1us; words.Add 0x6363us; words.Add 0x6D70us; words.Add 8us; words.Add 0us
            words.Add 2us; words.Add 0us; words.Add 1us
            words.Add 2us; words.Add 6us; words.Add (uint16 (6 + 2 * lookupLen))
            for i in 0 .. 1 do
                words.Add 2us; words.Add 0us; words.Add 1us; words.Add 8us
                words.Add 1us; words.Add 8us; words.Add 1us; words.Add 14us
                words.Add 1us; words.Add 1us; words.Add (uint16 (1 + i))
                words.Add (uint16 factor)
                for _ in 0 .. factor - 1 do
                    words.Add (uint16 (2 + i))
            let gsub = words |> Seq.collect (fun word -> be16 (uint32 word)) |> Array.ofSeq
            let cmap =
                Array.concat
                    [ be16 0u; be16 1u
                      be16 3u; be16 1u; be32 12u
                      be16 6u; be16 12u; be16 0u
                      be16 0x61u; be16 1u
                      be16 1u ]
            let maxp = Array.concat [ be32 0x00005000u; be16 4u ]
            let font =
                Font.create (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Gsub, gsub; Tags.Cmap, cmap; Tags.Maxp, maxp ])) 0u)
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x61u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withDirection Direction.Ltr
                |> GlyphRun.withScript Scripts.Latin
                |> TailCalls.settle
            match Shape.shape font ImmutableArray.Empty run with
            | Error error -> failtest $"expected Ok, got {error}"
            | Ok shaped -> Expect.equal shaped.Infos.Length factor "shaping stops at the buffer-length limit"
        }
        test "single substitution delta wraps modulo 0x10000" {
            let coverage = singleCoverage 0xFFFFu
            let subtable = SingleDelta (coverage, 1u, 0xFFFFu)
            let cursor = cursorOf (infoOf 0xFFFFu 0xFFFFFFFFu) (propsOf ()) 1u
            match GsubApply.trySubtable emptyGdef false 0xFFFFFFFFu false subtable &cursor with
            | GsubReplace (info, _) -> Expect.equal info.Codepoint 0u "0xFFFF + 1 wraps to 0"
            | _ -> failtest "expected a single substitution"
        }
        test "multiple substitution count 0 deletes the glyph" {
            let coverage = singleCoverage 5u
            let subtable = Multiple (coverage, Sealed.ofSpan (ReadOnlySpan [| (ImmutableArray.Empty: ImmutableArray<uint32>) |]))
            let cursor = cursorOf (infoOf 5u 0xFFFFFFFFu) (propsOf ()) 1u
            match GsubApply.trySubtable emptyGdef false 0xFFFFFFFFu false subtable &cursor with
            | GsubDelete -> ()
            | _ -> failtest "expected a deletion"
        }
        test "rand alternates pick randomly, rand=2 picks the second" {
            let coverage = singleCoverage 5u
            let alternates = Sealed.ofSpan (ReadOnlySpan [| glyphs [ 10u; 20u; 30u ] |])
            let subtable = Alternate (coverage, alternates)
            let cursor1 = cursorOf (infoOf 5u 2u) (propsOf ()) 1u
            match GsubApply.trySubtable emptyGdef false 0xFFu false subtable &cursor1 with
            | GsubReplace (info, _) -> Expect.equal info.Codepoint 20u "rand=2 selects alternate index 2"
            | _ -> failtest "expected a replacement"
            let cursor2 = cursorOf (infoOf 5u 0xFFu) (propsOf ()) 1u
            match GsubApply.trySubtable emptyGdef false 0xFFu true subtable &cursor2 with
            | GsubAlternateRandom (info, _, nextState) ->
                Expect.equal nextState 48271u "the random state advances once"
                Expect.equal info.Codepoint 20u "the first random alternate is index 2"
            | _ -> failtest "expected a random alternate"
        }
        test "single substitution non-matching not-gated allocates nothing" {
            let font = Font.create Face.empty
            let table = singleTableOf (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 0xFFFFu |]))) 1u
            let lookups = ImmutableArray.Create (lookupMapOf ())
            let stages = stageOf ()
            let run = maskedRunOf 1000
            let apply () = Engine.applyTable false (Engine.GsubApplier()) table lookups stages font run |> ignore
            Expect.equal (measureAlloc apply) 0L "a non-matching, not-gated lookup allocates nothing"
        }
        test "single substitution non-matching gated allocates nothing" {
            let font = Font.create Face.empty
            let table = singleTableOf (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 0xFFFFu |]))) 1u
            let lookups = ImmutableArray.Create (lookupMapOf ())
            let stages = stageOf ()
            let run = maskedRunOf 1000
            let apply () = Engine.applyTable true (Engine.GsubApplier()) table lookups stages font run |> ignore
            Expect.equal (measureAlloc apply) 0L "a non-matching, gated lookup allocates nothing"
        }
        test "single substitution replacing every glyph allocates only the output arrays" {
            let font = Font.create Face.empty
            let table = singleTableOf (CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })) 1u
            let lookups = ImmutableArray.Create (lookupMapOf ())
            let stages = stageOf ()
            let run = maskedRunOf 1000
            let apply () = Engine.applyTable false (Engine.GsubApplier()) table lookups stages font run |> ignore
            let allocated = measureAlloc apply
            let outputBytes = int64 (1000 * 4 * (Unsafe.SizeOf<GlyphInfo>() + Unsafe.SizeOf<GlyphProps>())) + 4096L
            Expect.isLessThanOrEqual allocated outputBytes $"replacing every glyph allocates at most the output arrays (allocated {allocated})"
        }
    ]
