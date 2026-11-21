module Farfbuzz.Tests.Layout.GposAttachTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The number of random cases of the synthetic property.</summary>
[<Literal>]
let private SyntheticCaseCount = 8000

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x5A11CEUL

/// <summary>A big-endian unsigned 16-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u16 (value: int) = [| byte (value >>> 8); byte value |]

/// <summary>A big-endian unsigned 24-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u24 (value: int) = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A big-endian unsigned 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u32 (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A coverage of format 1 over sorted glyphs.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The bytes.</returns>
let private coverageBytes (glyphs: int list) =
    Array.concat [ u16 1; u16 glyphs.Length; Array.concat (glyphs |> List.map u16) ]

/// <summary>An anchor of format 1.</summary>
/// <param name="x">The x coordinate.</param>
/// <param name="y">The y coordinate.</param>
/// <returns>The bytes.</returns>
let private anchor1 (x: int) (y: int) = Array.concat [ u16 1; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF) ]

/// <summary>An anchor of format 2.</summary>
/// <param name="x">The x coordinate.</param>
/// <param name="y">The y coordinate.</param>
/// <param name="point">The contour point.</param>
/// <returns>The bytes.</returns>
let private anchor2 (x: int) (y: int) (point: int) = Array.concat [ u16 2; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF); u16 point ]

/// <summary>An anchor of format 3 with the offsets of its devices; the devices follow the anchor, a hinting device for 11 to 13
/// ppem whose only delta is one pixel for ppem 11 and 12, then the variation index (0, 0).</summary>
/// <param name="x">The x coordinate.</param>
/// <param name="y">The y coordinate.</param>
/// <returns>The bytes: the anchor, the x device and the y device.</returns>
let private anchor3 (x: int) (y: int) =
    let hinting = Array.concat [ u16 11; u16 13; u16 1; u16 0x4000 ]
    let variation = Array.concat [ u16 0; u16 0; u16 0x8000 ]
    Array.concat [ u16 3; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF); u16 10; u16 (10 + hinting.Length); hinting; variation ]

/// <summary>Wraps one subtable in a GPOS table of one lookup, with no scripts or features.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="flag">The lookup flag.</param>
/// <param name="subtable">The subtable bytes.</param>
/// <returns>The table bytes.</returns>
let private gposOf (lookupType: int) (flag: int) (subtable: byte array) =
    let lookup = Array.concat [ u16 lookupType; u16 flag; u16 1; u16 8; subtable ]
    let lookupList = Array.concat [ u16 1; u16 4; lookup ]
    Array.concat [ u32 0x00010000; u16 10; u16 12; u16 14; u16 0; u16 0; lookupList ]

/// <summary>Loads the GPOS table of a font made of these bytes.</summary>
/// <param name="gpos">The table bytes.</param>
/// <returns>The table, empty when the sanitizer rejected it.</returns>
let private loadGpos (gpos: byte array) =
    let file = SfntBuilder.sfnt 0x00010000u [ Tags.Gpos, gpos ]
    (OtFace.create (Face.create (ReadOnlyMemory file) 0u)).Gpos.Value

/// <summary>A mark-to-base subtable of format 1: two marks (classes 0 and 1), two bases, two classes. The mark anchors are
/// (10, 20) and (30, 40) and the base matrix holds (110, 120), (130, 140), (210, 220) and (230, 240).</summary>
/// <returns>The bytes.</returns>
let private markBaseBytes () =
    let markCoverage = coverageBytes [ 5; 6 ]
    let baseCoverage = coverageBytes [ 1; 2 ]
    let markAnchors = [ anchor1 10 20; anchor1 30 40 ]
    let markArray =
        let head = 2 + 2 * 4
        Array.concat [ u16 2; u16 0; u16 head; u16 1; u16 (head + markAnchors[0].Length); markAnchors[0]; markAnchors[1] ]
    let baseAnchors = [ anchor1 110 120; anchor1 130 140; anchor1 210 220; anchor1 230 240 ]
    let baseArray =
        let head = 2 + 4 * 2
        let offsets = baseAnchors |> List.fold (fun (offsets, at) anchor -> offsets @ [ at ], at + anchor.Length) ([], head) |> fst
        Array.concat [ u16 2; Array.concat (offsets |> List.map u16); Array.concat baseAnchors ]
    let header = 12
    let markCoverageAt = header
    let baseCoverageAt = markCoverageAt + markCoverage.Length
    let markArrayAt = baseCoverageAt + baseCoverage.Length
    let baseArrayAt = markArrayAt + markArray.Length
    Array.concat [ u16 1; u16 markCoverageAt; u16 baseCoverageAt; u16 2; u16 markArrayAt; u16 baseArrayAt; markCoverage; baseCoverage; markArray; baseArray ]

/// <summary>The scale and ppem of a font whose design coordinates read as the test says.</summary>
/// <param name="scale">The scale.</param>
/// <param name="ppem">The ppem.</param>
/// <returns>The font.</returns>
let private fontOf (scale: int) (ppem: uint32) =
    Font.create Face.empty |> Font.withScale scale scale |> Font.withPpem ppem ppem

/// <summary>The environment of a lookup with the given flags on a font and a direction.</summary>
/// <param name="font">The font.</param>
/// <param name="direction">The direction.</param>
/// <param name="props">The lookup flags.</param>
/// <returns>The environment.</returns>
let private envOf (font: Font) (direction: Direction) (props: uint32) =
    { Font = font
      Direction = direction
      Horizontal = Direction.isHorizontal direction
      Matcher = Matching.matcherFor 1 false props 1u false false false 0uy
      Gdef = font.Ot.Gdef.Value }

/// <summary>Glyph infos under mask 1, one cluster each.</summary>
/// <param name="glyphs">The glyph ids.</param>
/// <returns>The infos.</returns>
let private infosOf (glyphs: uint32 array) =
    glyphs |> Array.mapi (fun index glyph -> { Codepoint = glyph; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })

/// <summary>Glyph properties of the given classes.</summary>
/// <param name="classes">The glyph classes.</param>
/// <returns>The properties.</returns>
let private propsOf (classes: uint16 array) =
    classes |> Array.map (fun glyphClass -> { Unchecked.defaultof<GlyphProps> with GlyphClass = glyphClass })

/// <summary>Zeroed positions with an advance of 500.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The positions.</returns>
let private positionsOf (count: int) =
    Array.init count (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })

/// <summary>A coverage over some glyphs, for a subtable built by hand.</summary>
/// <param name="glyphs">The glyphs, sorted.</param>
/// <returns>The coverage.</returns>
let private coverageOf (glyphs: uint32 array) =
    CoverageGlyphs (ImmutableArray.CreateRange glyphs)

/// <summary>A direct table that sends every query to the coverage's structure.</summary>
let private structural = { Table = ImmutableArray<uint16>.Empty; Complete = false }

/// <summary>A mark-to-base or mark-to-mark subtable built by hand.</summary>
/// <param name="marks">The mark glyphs.</param>
/// <param name="targets">The base glyphs.</param>
/// <param name="classCount">The class count.</param>
/// <param name="markAnchors">The class and anchor of each mark.</param>
/// <param name="rows">The number of rows.</param>
/// <param name="anchors">The matrix.</param>
/// <returns>The subtable.</returns>
let private markAttachmentOf (marks: uint32 array) (targets: uint32 array) (classCount: int) (markAnchors: (int * Anchor) array) (rows: int) (anchors: Anchor array) =
    { MarkCoverage = coverageOf marks
      MarkDirect = structural
      TargetCoverage = coverageOf targets
      TargetDirect = structural
      ClassCount = classCount
      MarkClasses = ImmutableArray.CreateRange (markAnchors |> Array.map (fun (klass, _) -> uint16 klass))
      MarkAnchors = ImmutableArray.CreateRange (markAnchors |> Array.map snd)
      Rows = rows
      Anchors = ImmutableArray.CreateRange anchors }

/// <summary>Builds a lookup of one type from subtables.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="props">The lookup flags.</param>
/// <param name="subtables">The subtables.</param>
/// <returns>The lookup.</returns>
let private lookupOf (lookupType: int) (props: uint32) (subtables: PosSubtable list) =
    LayoutAccel.build GposTables.subtableDigest GposTables.secondDigest
        { LookupType = lookupType
          Props = props
          IsReverse = false
          Subtables = ImmutableArray.CreateRange subtables }

/// <summary>Builds a GPOS table of lookups.</summary>
/// <param name="lookups">The lookups.</param>
/// <returns>The table.</returns>
let private tableOf (lookups: LookupAccel<PosSubtable> list) =
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = lookups |> List.map (fun lookup -> lazy lookup) |> ImmutableArray.CreateRange
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>The map that applies the first lookups of a table under mask 1, with a single stage.</summary>
/// <param name="count">The number of lookups.</param>
/// <returns>The lookups and the stages.</returns>
let private mapOf (count: int) =
    let maps =
        Array.init count (fun index ->
            { LookupIndex = index
              AutoZwnj = false
              AutoZwj = false
              Random = false
              PerSyllable = false
              Mask = 1u
              FeatureTag = Tag.make 'm' 'a' 'r' 'k' })
    struct (ImmutableArray.CreateRange maps, ImmutableArray.Create { LastLookup = count; Pause = PauseKind.NoPause })

/// <summary>A run of infos, props and positions in a direction.</summary>
/// <param name="infos">The infos.</param>
/// <param name="props">The properties.</param>
/// <param name="positions">The positions.</param>
/// <param name="direction">The direction.</param>
/// <returns>The run.</returns>
let private runOf (infos: GlyphInfo array) (props: GlyphProps array) (positions: GlyphPosition array) (direction: Direction) =
    { GlyphRun.empty with
        Infos = ImmutableArray.CreateRange infos
        Props = ImmutableArray.CreateRange props
        Positions = ImmutableArray.CreateRange positions
        HavePositions = true
        ContentType = GlyphContent
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale } }

/// <summary>Positions a run through the kernel with the table's lookups, with the sets of the map and without them.</summary>
/// <param name="font">The font.</param>
/// <param name="table">The table.</param>
/// <param name="run">The run.</param>
/// <returns>The positioned run.</returns>
let private position (font: Font) (table: LayoutTable<PosSubtable>) (run: GlyphRun) =
    let struct (maps, stages) = mapOf table.Lookups.Length
    let gdef = font.Ot.Gdef.Value
    let covered = Engine.gposLookupCoveredOf table maps
    let glyphLookups = Engine.gposGlyphLookupsOf table maps
    let planned = Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run
    let ungated = Engine.positionTable font table gdef maps stages ImmutableArray.Empty ImmutableArray.Empty ImmutableArray.Empty run
    expectSameRunAll "the kernel through the plan's sets equals the kernel without them" ungated planned
    planned

/// <summary>The literal transliteration of the upstream apply functions the kernel is held to: one mutable buffer, every
/// function a line-by-line copy of its C++ original, no gate and no digest.</summary>
module private Reference =
    /// <summary>The state of <c>hb_buffer_t</c> and <c>hb_ot_apply_context_t</c> the attachment lookups read and write.</summary>
    [<NoComparison; NoEquality>]
    type State =
        { Info: GlyphInfo array
          Props: GlyphProps array
          Pos: GlyphPosition array
          Len: int
          Flags: BufferFlags
          Level: ClusterLevel
          Direction: Direction
          Font: Font
          Gdef: Gdef
          mutable Idx: int
          mutable MaxOps: int
          mutable Successful: bool
          mutable Scratch: ScratchFlags
          mutable LookupMask: uint32
          mutable LookupProps: uint32
          mutable IterProps: uint32
          mutable AutoZwnj: bool
          mutable AutoZwj: bool
          mutable LastBase: int
          mutable LastBaseUntil: int }

    /// <summary>The attach chain, <c>attach_chain()</c>.</summary>
    let chain (position: GlyphPosition) = int (int16 (position.Var &&& 0xFFFFu))

    /// <summary>The attach type, <c>attach_type()</c>.</summary>
    let attachType (position: GlyphPosition) = int ((position.Var >>> 16) &&& 0xFFu)

    /// <summary>Stores the attach chain through its 16-bit field.</summary>
    let setChain (state: State) (index: int) (value: int) =
        state.Pos[index] <- { state.Pos[index] with Var = (state.Pos[index].Var &&& 0xFFFF0000u) ||| uint32 (uint16 value) }

    /// <summary>Stores the attach type through its byte.</summary>
    let setType (state: State) (index: int) (value: int) =
        state.Pos[index] <- { state.Pos[index] with Var = (state.Pos[index].Var &&& 0xFF00FFFFu) ||| (uint32 (byte value) <<< 16) }

    /// <summary>The saturating sum, <c>hb_saturate_add</c>.</summary>
    let satAdd (a: int) (b: int) =
        let r = int64 a + int64 b
        if r > int64 Int32.MaxValue then Int32.MaxValue elif r < int64 Int32.MinValue then Int32.MinValue else int r

    /// <summary>The saturating difference, <c>hb_saturate_sub</c>.</summary>
    let satSub (a: int) (b: int) =
        let r = int64 a - int64 b
        if r > int64 Int32.MaxValue then Int32.MaxValue elif r < int64 Int32.MinValue then Int32.MinValue else int r

    /// <summary>The C conversion of a rounded float to a position.</summary>
    let toInt (x: float32) =
        let d = float x
        if Double.IsNaN d || d < float Int32.MinValue then Int32.MinValue elif d > float Int32.MaxValue then Int32.MaxValue else int d

    /// <summary><c>_hb_roundf</c>: halves round towards positive infinity.</summary>
    let roundf (x: float32) = float32 (Math.Floor (float x + 0.5))

    /// <summary>The coverage index of a glyph, or the all-ones value of <c>NOT_COVERED</c> as minus one.</summary>
    let covered (coverage: Coverage) (glyph: uint32) =
        match Coverage.index coverage glyph with
        | Covered index -> index
        | NotCovered -> -1

    /// <summary>The ligature id, <c>_hb_glyph_info_get_lig_id</c>.</summary>
    let ligId (p: GlyphProps) = int (p.LigProps >>> 5)

    /// <summary>The ligature component, <c>_hb_glyph_info_get_lig_comp</c>.</summary>
    let ligComp (p: GlyphProps) = if p.LigProps &&& 0x10uy <> 0uy then 0 else int (p.LigProps &&& 0x0Fuy)

    /// <summary>The multiplied bit, <c>_hb_glyph_info_multiplied</c>.</summary>
    let multiplied (p: GlyphProps) = p.GlyphClass &&& 0x40us <> 0us

    /// <summary>The mark bit, <c>_hb_glyph_info_is_mark</c>.</summary>
    let isMark (p: GlyphProps) = p.GlyphClass &&& 0x08us <> 0us

    /// <summary><c>_infos_find_min_cluster</c>.</summary>
    let findMinCluster (state: State) (start: int) (finish: int) (cluster: uint32) =
        if start = finish then cluster
        elif state.Level = ClusterLevel.Characters then
            let mutable least = cluster
            for i in start .. finish - 1 do
                least <- min least state.Info[i].Cluster
            least
        else min cluster (min state.Info[start].Cluster state.Info[finish - 1].Cluster)

    /// <summary><c>_infos_set_glyph_flags</c>.</summary>
    let setInfosFlags (state: State) (start: int) (finish: int) (cluster: uint32) (mask: uint32) =
        if start <> finish then
            state.MaxOps <- state.MaxOps - (finish - start)
            if state.MaxOps < 0 then state.Successful <- false
            let clusterFirst = state.Info[start].Cluster
            let clusterLast = state.Info[finish - 1].Cluster
            if state.Level = ClusterLevel.Characters || (cluster <> clusterFirst && cluster <> clusterLast) then
                for i in start .. finish - 1 do
                    if cluster <> state.Info[i].Cluster then state.Info[i] <- { state.Info[i] with Mask = state.Info[i].Mask ||| mask }
            elif cluster = clusterFirst then
                let mutable i = finish
                while start < i && state.Info[i - 1].Cluster <> clusterFirst do
                    state.Info[i - 1] <- { state.Info[i - 1] with Mask = state.Info[i - 1].Mask ||| mask }
                    i <- i - 1
            else
                let mutable i = start
                while i < finish && state.Info[i].Cluster <> clusterLast do
                    state.Info[i] <- { state.Info[i] with Mask = state.Info[i].Mask ||| mask }
                    i <- i + 1

    /// <summary><c>_set_glyph_flags</c> without an out buffer.</summary>
    let setGlyphFlags (state: State) (mask: uint32) (start: int) (finish: int) (interior: bool) =
        if not (uint32 finish - uint32 start > 255u) then
            let finish = min finish state.Len
            if not (interior && finish - start < 2) then
                if not interior then
                    for i in start .. finish - 1 do
                        state.Info[i] <- { state.Info[i] with Mask = state.Info[i].Mask ||| mask }
                else
                    let cluster = findMinCluster state start finish UInt32.MaxValue
                    setInfosFlags state start finish cluster mask

    /// <summary><c>unsafe_to_break</c>.</summary>
    let unsafeToBreak (state: State) (start: int) (finish: int) = setGlyphFlags state 3u start finish true

    /// <summary><c>unsafe_to_concat_from_outbuffer</c>.</summary>
    let unsafeToConcatFromOutbuffer (state: State) (start: int) (finish: int) =
        if state.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default then setGlyphFlags state 2u start finish false

    /// <summary><c>get_anchor</c> of the three anchor formats; a contour point is never found by the OpenType font funcs, and the
    /// test fonts have no variation coordinates, so a device counts when the font has a ppem.</summary>
    let anchorXY (state: State) (anchor: Anchor) =
        let font = state.Font
        match anchor with
        | NoAnchor -> 0f, 0f
        | PlainAnchor (x, y) -> Font.emFscaleX font x, Font.emFscaleY font y
        | DeviceAnchor (x, y, xDevice, yDevice) ->
            let useX = font.XPpem <> 0u
            let useY = font.YPpem <> 0u
            let baseX = Font.emFscaleX font x
            let baseY = Font.emFscaleY font y
            (if useX then baseX + Device.xDeltaF font xDevice else baseX), (if useY then baseY + Device.yDeltaF font yDevice else baseY)

    /// <summary>The iterator's matcher, <c>matcher_t::init</c> with the iterator's own lookup properties.</summary>
    let matcher (state: State) =
        Matching.matcherFor 1 false state.IterProps state.LookupMask state.AutoZwnj state.AutoZwj false 0uy

    /// <summary><c>skipping_iterator_t::match</c>: true for a match.</summary>
    let isMatch (state: State) (index: int) =
        match Matching.verdict (matcher state) state.Gdef (NoMatchFunc()) 0 state.Info[index] state.Props[index] with
        | Matched -> true
        | NotMatched -> false
        | Skipped -> false

    /// <summary><c>skipping_iterator_t::prev</c>: the index found, or the unsafe bound of a miss as a negative number below minus one.</summary>
    let prev (state: State) (start: int) =
        let mutable idx = start
        let mutable result = None
        while result.IsNone && idx > 0 do
            idx <- idx - 1
            match Matching.verdict (matcher state) state.Gdef (NoMatchFunc()) 0 state.Info[idx] state.Props[idx] with
            | Matched -> result <- Some (Choice1Of2 idx)
            | NotMatched -> result <- Some (Choice2Of2 (max 1 idx - 1))
            | Skipped -> ()
        match result with
        | Some found -> found
        | None -> Choice2Of2 0

    /// <summary><c>resolve_cross_offset</c>.</summary>
    let resolveCrossOffset (state: State) (glyphPos: int) =
        let horizontal = Direction.isHorizontal state.Direction
        let mutable glyph = glyphPos
        let mutable offset = if horizontal then state.Pos[glyph].YOffset else state.Pos[glyph].XOffset
        let mutable count = 1
        let mutable go = true
        while go && count < state.Len && (attachType state.Pos[glyph] &&& 2) <> 0 do
            let c = chain state.Pos[glyph]
            if c = 0 then go <- false
            else
                let parent = uint32 (glyph + c)
                if parent >= uint32 state.Len then go <- false
                else
                    glyph <- int parent
                    offset <- satAdd offset (if horizontal then state.Pos[glyph].YOffset else state.Pos[glyph].XOffset)
                    count <- count + 1
        offset

    /// <summary><c>apply_mark_attachment</c>.</summary>
    let applyMarkAttachment (state: State) (markAnchor: Anchor) (glyphAnchor: Anchor) (glyphPos: int) =
        unsafeToBreak state glyphPos (state.Idx + 1)
        let markX, markY = anchorXY state markAnchor
        let baseX, baseY = anchorXY state glyphAnchor
        let baseOffset = resolveCrossOffset state glyphPos
        let delta = glyphPos - state.Idx
        setChain state state.Idx delta
        if chain state.Pos[state.Idx] <> delta then setChain state state.Idx 0
        else
            setType state state.Idx 1
            state.Pos[state.Idx] <- { state.Pos[state.Idx] with XOffset = toInt (roundf (baseX - markX)); YOffset = toInt (roundf (baseY - markY)) }
            if Direction.isHorizontal state.Direction then state.Pos[state.Idx] <- { state.Pos[state.Idx] with YOffset = satAdd state.Pos[state.Idx].YOffset baseOffset }
            else state.Pos[state.Idx] <- { state.Pos[state.Idx] with XOffset = satAdd state.Pos[state.Idx].XOffset baseOffset }
            state.Scratch <- state.Scratch ||| ScratchFlags.HasGposAttachment
        state.Idx <- state.Idx + 1
        true

    /// <summary><c>MarkArray::apply</c> over the mark array of a subtable.</summary>
    let applyMarkArray (state: State) (classes: ImmutableArray<uint16>) (anchors: ImmutableArray<Anchor>) (markIndex: int) (matrixAnchor: int -> int -> Anchor option) (classCount: int) (glyphPos: int) =
        let markClass = if markIndex < classes.Length then int classes[markIndex] else 0
        let markAnchor = if markIndex < anchors.Length then anchors[markIndex] else PlainAnchor (0, 0)
        match matrixAnchor markClass classCount with
        | None -> false
        | Some glyphAnchor -> applyMarkAttachment state markAnchor glyphAnchor glyphPos

    /// <summary><c>AnchorMatrix::get_anchor</c>: none when not found.</summary>
    let getAnchor (anchors: ImmutableArray<Anchor>) (start: int) (rows: int) (row: int) (column: int) (columns: int) =
        if row >= rows || column >= columns then None
        else
            let at = start + row * columns + column
            if at >= anchors.Length then None
            else
                match anchors[at] with
                | NoAnchor -> None
                | found -> Some found

    /// <summary>The search of <c>MarkBasePosFormat1_2::apply</c> and <c>MarkLigPosFormat1_2::apply</c>: the index of the base found, or minus one.</summary>
    let searchBase (state: State) (targetCoverage: Coverage) (accept: int -> bool) =
        state.IterProps <- 0x0008u
        if state.LastBaseUntil > state.Idx then
            state.LastBaseUntil <- 0
            state.LastBase <- -1
        let mutable j = state.Idx
        let mutable found = false
        while not found && j > state.LastBaseUntil do
            let matched = isMatch state (j - 1)
            let selected = matched && (accept (j - 1) || covered targetCoverage state.Info[j - 1].Codepoint >= 0)
            if selected then
                state.LastBase <- j - 1
                found <- true
            else j <- j - 1
        state.LastBaseUntil <- state.Idx
        state.LastBase

    /// <summary><c>MarkBasePosFormat1_2::apply</c>.</summary>
    let applyMarkBase (state: State) (subtable: MarkAttachment) =
        let markIndex = covered subtable.MarkCoverage state.Info[state.Idx].Codepoint
        if markIndex < 0 then false
        else
            let accept (idx: int) =
                let here = state.Props[idx]
                not (multiplied here)
                || ligComp here = 0
                || (idx = 0
                    || isMark state.Props[idx - 1]
                    || not (multiplied state.Props[idx - 1])
                    || ligId here <> ligId state.Props[idx - 1]
                    || ligComp here <> ligComp state.Props[idx - 1] + 1)
            let baseIndex = searchBase state subtable.TargetCoverage accept
            if baseIndex = -1 then
                unsafeToConcatFromOutbuffer state 0 (state.Idx + 1)
                false
            else
                let row = covered subtable.TargetCoverage state.Info[baseIndex].Codepoint
                if row < 0 then
                    unsafeToConcatFromOutbuffer state baseIndex (state.Idx + 1)
                    false
                else applyMarkArray state subtable.MarkClasses subtable.MarkAnchors markIndex (fun klass columns -> getAnchor subtable.Anchors 0 subtable.Rows row klass columns) subtable.ClassCount baseIndex

    /// <summary><c>MarkLigPosFormat1_2::apply</c>.</summary>
    let applyMarkLigature (state: State) (subtable: LigatureAttachment) =
        let markIndex = covered subtable.LigMarkCoverage state.Info[state.Idx].Codepoint
        if markIndex < 0 then false
        else
            let accept (idx: int) = not (multiplied state.Props[idx]) || ligComp state.Props[idx] = 0
            let baseIndex = searchBase state subtable.LigatureCoverage accept
            if baseIndex = -1 then
                unsafeToConcatFromOutbuffer state 0 (state.Idx + 1)
                false
            else
                let ligIndex = covered subtable.LigatureCoverage state.Info[baseIndex].Codepoint
                if ligIndex < 0 then
                    unsafeToConcatFromOutbuffer state baseIndex (state.Idx + 1)
                    false
                else
                    let compCount = if ligIndex < subtable.Components.Length then subtable.Components[ligIndex] else 0
                    if compCount = 0 then
                        unsafeToConcatFromOutbuffer state baseIndex (state.Idx + 1)
                        false
                    else
                        let ligIdOfBase = ligId state.Props[baseIndex]
                        let markId = ligId state.Props[state.Idx]
                        let markComp = ligComp state.Props[state.Idx]
                        let compIndex = if ligIdOfBase <> 0 && ligIdOfBase = markId && markComp > 0 then min compCount markComp - 1 else compCount - 1
                        applyMarkArray state subtable.LigMarkClasses subtable.LigMarkAnchors markIndex (fun klass columns -> getAnchor subtable.LigAnchors subtable.Starts[ligIndex] compCount compIndex klass columns) subtable.LigClassCount baseIndex

    /// <summary><c>MarkMarkPosFormat1_2::apply</c>.</summary>
    let applyMarkMark (state: State) (subtable: MarkAttachment) =
        let mark1Index = covered subtable.MarkCoverage state.Info[state.Idx].Codepoint
        if mark1Index < 0 then false
        else
            state.IterProps <- state.LookupProps &&& ~~~0x000Eu
            match prev state state.Idx with
            | Choice2Of2 unsafeFrom ->
                unsafeToConcatFromOutbuffer state unsafeFrom (state.Idx + 1)
                false
            | Choice1Of2 j ->
                if not (isMark state.Props[j]) then
                    unsafeToConcatFromOutbuffer state j (state.Idx + 1)
                    false
                else
                    let id1 = ligId state.Props[state.Idx]
                    let id2 = ligId state.Props[j]
                    let comp1 = ligComp state.Props[state.Idx]
                    let comp2 = ligComp state.Props[j]
                    let good =
                        if id1 = id2 then id1 = 0 || comp1 = comp2
                        else (id1 > 0 && comp1 = 0) || (id2 > 0 && comp2 = 0)
                    if not good then
                        unsafeToConcatFromOutbuffer state j (state.Idx + 1)
                        false
                    else
                        let mark2Index = covered subtable.TargetCoverage state.Info[j].Codepoint
                        if mark2Index < 0 then
                            unsafeToConcatFromOutbuffer state j (state.Idx + 1)
                            false
                        else applyMarkArray state subtable.MarkClasses subtable.MarkAnchors mark1Index (fun klass columns -> getAnchor subtable.Anchors 0 subtable.Rows mark2Index klass columns) subtable.ClassCount j

    /// <summary><c>reverse_cursive_minor_offset</c>.</summary>
    let rec reverseCursiveMinorOffset (state: State) (i: int) (newParent: int) (nesting: int) =
        if nesting <= 64 then
            let c = chain state.Pos[i]
            let t = attachType state.Pos[i]
            if not (c = 0 || (t &&& 2) = 0) then
                setChain state i 0
                let j = uint32 (i + c)
                if not (j >= uint32 state.Len) && int j <> newParent then
                    let reversed = int (int16 (-c))
                    if reversed = -c then
                        reverseCursiveMinorOffset state (int j) newParent (nesting + 1)
                        if Direction.isHorizontal state.Direction then state.Pos[int j] <- { state.Pos[int j] with YOffset = satSub 0 state.Pos[i].YOffset }
                        else state.Pos[int j] <- { state.Pos[int j] with XOffset = satSub 0 state.Pos[i].XOffset }
                        setChain state (int j) reversed
                        setType state (int j) t

    /// <summary><c>apply_cursive_attachment</c>.</summary>
    let applyCursiveAttachment (state: State) (exitAnchor: Anchor) (entryAnchor: Anchor) (i: int) =
        let j = state.Idx
        unsafeToBreak state i (j + 1)
        let exitX, exitY = anchorXY state exitAnchor
        let entryX, entryY = anchorXY state entryAnchor
        let pos = state.Pos
        match state.Direction with
        | Direction.Ltr ->
            pos[i] <- { pos[i] with XAdvance = satAdd (toInt (roundf exitX)) pos[i].XOffset }
            let d = satAdd (toInt (roundf entryX)) pos[j].XOffset
            pos[j] <- { pos[j] with XAdvance = satSub pos[j].XAdvance d; XOffset = satSub pos[j].XOffset d }
        | Direction.Rtl ->
            let d = satAdd (toInt (roundf exitX)) pos[i].XOffset
            pos[i] <- { pos[i] with XAdvance = satSub pos[i].XAdvance d; XOffset = satSub pos[i].XOffset d }
            pos[j] <- { pos[j] with XAdvance = satAdd (toInt (roundf entryX)) pos[j].XOffset }
        | Direction.Ttb ->
            pos[i] <- { pos[i] with YAdvance = satAdd (toInt (roundf exitY)) pos[i].YOffset }
            let d = satAdd (toInt (roundf entryY)) pos[j].YOffset
            pos[j] <- { pos[j] with YAdvance = satSub pos[j].YAdvance d; YOffset = satSub pos[j].YOffset d }
        | Direction.Btt ->
            let d = satAdd (toInt (roundf exitY)) pos[i].YOffset
            pos[i] <- { pos[i] with YAdvance = satSub pos[i].YAdvance d; YOffset = satSub pos[i].YOffset d }
            pos[j] <- { pos[j] with YAdvance = toInt (roundf entryY) }
        | Direction.Invalid -> ()
        let mutable child = i
        let mutable parent = j
        let mutable xOffset = toInt (roundf (entryX - exitX))
        let mutable yOffset = toInt (roundf (entryY - exitY))
        if state.LookupProps &&& 1u = 0u then
            let k = child
            child <- parent
            parent <- k
            xOffset <- -xOffset
            yOffset <- -yOffset
        reverseCursiveMinorOffset state child parent 0
        setChain state child (parent - child)
        if chain state.Pos[child] <> parent - child then setChain state child 0
        else
            setType state child 2
            state.Scratch <- state.Scratch ||| ScratchFlags.HasGposAttachment
            if Direction.isHorizontal state.Direction then state.Pos[child] <- { state.Pos[child] with YOffset = yOffset }
            else state.Pos[child] <- { state.Pos[child] with XOffset = xOffset }
            if chain state.Pos[parent] = -(chain state.Pos[child]) then
                setChain state parent 0
                if Direction.isHorizontal state.Direction then state.Pos[parent] <- { state.Pos[parent] with YOffset = 0 }
                else state.Pos[parent] <- { state.Pos[parent] with XOffset = 0 }
        state.Idx <- state.Idx + 1
        true

    /// <summary><c>CursivePosFormat1_2::apply</c>.</summary>
    let applyCursive (state: State) (subtable: CursiveAttachment) =
        let record (anchors: ImmutableArray<Anchor>) (glyph: uint32) =
            let index = covered subtable.CursiveCoverage glyph
            if index >= 0 && index < anchors.Length then anchors[index] else NoAnchor
        match record subtable.Entries state.Info[state.Idx].Codepoint with
        | NoAnchor -> false
        | entry ->
            match prev state state.Idx with
            | Choice2Of2 unsafeFrom ->
                unsafeToConcatFromOutbuffer state unsafeFrom (state.Idx + 1)
                false
            | Choice1Of2 previous ->
                match record subtable.Exits state.Info[previous].Codepoint with
                | NoAnchor ->
                    unsafeToConcatFromOutbuffer state previous (state.Idx + 1)
                    false
                | exitAnchor -> applyCursiveAttachment state exitAnchor entry previous

    /// <summary>One subtable's apply, whatever its type.</summary>
    let applySubtable (state: State) (subtable: PosSubtable) =
        match subtable with
        | CursivePos cursive -> applyCursive state cursive
        | MarkBasePos attachment -> applyMarkBase state attachment
        | MarkLigPos ligature -> applyMarkLigature state ligature
        | MarkMarkPos attachment -> applyMarkMark state attachment
        | SinglePos _ -> false
        | PairPosGlyphs _ -> false
        | PairPosClasses _ -> false
        | PosContext _ -> false
        | PosChainContext _ -> false
        | PosPending _ -> false

    /// <summary><c>check_glyph_property</c> of the current glyph.</summary>
    let checkGlyphProperty (state: State) (index: int) =
        Matching.checkGlyphProperty state.Gdef state.Info[index].Codepoint state.Props[index].GlyphClass state.LookupProps

    /// <summary><c>apply_forward</c> over one lookup.</summary>
    let applyForward (state: State) (accel: LookupAccel<PosSubtable>) =
        state.Idx <- 0
        let mutable running = true
        while running && state.Successful do
            let mutable j = state.Idx
            while j < state.Len && not ((state.Info[j].Mask &&& state.LookupMask) <> 0u && checkGlyphProperty state j) do
                j <- j + 1
            state.Idx <- j
            if state.Idx >= state.Len then running <- false
            else
                let applied = accel.Subtables |> Seq.exists (fun subtable -> applySubtable state subtable.Subtable)
                if not applied then state.Idx <- state.Idx + 1

    /// <summary>Positions a run as <c>hb_ot_map_t::apply</c> does for GPOS, every lookup of the map in order.</summary>
    /// <param name="font">The font.</param>
    /// <param name="table">The table.</param>
    /// <param name="maps">The map's lookups.</param>
    /// <param name="stages">The map's stages.</param>
    /// <param name="run">The run.</param>
    /// <returns>The state after the lookups.</returns>
    let position (font: Font) (table: LayoutTable<PosSubtable>) (maps: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (run: GlyphRun) =
        let len = run.Infos.Length
        let usable = run.Successful && run.HavePositions && run.Positions.Length = len && run.Props.Length = len
        let state =
            { Info = run.Infos.AsSpan().ToArray()
              Props = run.Props.AsSpan().ToArray()
              Pos = run.Positions.AsSpan().ToArray()
              Len = (if usable then len else 0)
              Flags = run.Flags
              Level = run.ClusterLevel
              Direction = run.SegmentProperties.Direction
              Font = font
              Gdef = font.Ot.Gdef.Value
              Idx = 0
              MaxOps = run.MaxOps
              Successful = run.Successful
              Scratch = run.ScratchFlags
              LookupMask = 1u
              LookupProps = 0u
              IterProps = 0u
              AutoZwnj = true
              AutoZwj = true
              LastBase = -1
              LastBaseUntil = 0 }
        let mutable i = 0
        for stage in stages do
            while i < stage.LastLookup do
                let lookup = maps[i]
                let accel = table.Lookups[lookup.LookupIndex].Value
                if state.Len > 0 && lookup.Mask <> 0u then
                    state.LookupMask <- lookup.Mask
                    state.LastBase <- -1
                    state.LastBaseUntil <- 0
                    state.AutoZwj <- lookup.AutoZwj
                    state.AutoZwnj <- lookup.AutoZwnj
                    state.LookupProps <- Gdef.sanitizeLookupProps state.Gdef accel.Props
                    state.IterProps <- state.LookupProps
                    applyForward state accel
                i <- i + 1
        state

/// <summary>Checks a kernel result against the literal transliteration in every field.</summary>
/// <param name="what">What the case is, for the failure message.</param>
/// <param name="font">The font.</param>
/// <param name="table">The table.</param>
/// <param name="maps">The map's lookups.</param>
/// <param name="stages">The map's stages.</param>
/// <param name="run">The run.</param>
/// <returns>The positioned run of the kernel.</returns>
let private checkAgainstReference (what: string) (font: Font) (table: LayoutTable<PosSubtable>) (maps: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (run: GlyphRun) =
    let gdef = font.Ot.Gdef.Value
    let covered = Engine.gposLookupCoveredOf table maps
    let glyphLookups = Engine.gposGlyphLookupsOf table maps
    let kernel = Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run
    let ungated = Engine.positionTable font table gdef maps stages ImmutableArray.Empty ImmutableArray.Empty ImmutableArray.Empty run
    expectSameRunAll $"{what}: through the plan's sets and without them" ungated kernel
    let reference = Reference.position font table maps stages run
    Expect.sequenceEqual (kernel.Infos.AsSpan().ToArray()) reference.Info $"{what}: infos"
    Expect.sequenceEqual (kernel.Positions.AsSpan().ToArray()) reference.Pos $"{what}: positions, var included"
    Expect.equal kernel.MaxOps reference.MaxOps $"{what}: operation budget"
    Expect.equal kernel.Successful reference.Successful $"{what}: successful"
    Expect.equal kernel.ScratchFlags reference.Scratch $"{what}: scratch flags"
    Expect.equal kernel.Props.Length kernel.Infos.Length $"{what}: a props entry for each glyph"
    Expect.sequenceEqual (kernel.Props.AsSpan().ToArray()) (run.Props.AsSpan().ToArray()) $"{what}: props"
    kernel

/// <summary>A hinting device, whose delta at ppem 12 is one pixel.</summary>
let private hinting = HintingDevice (11us, 13us, 1us, ReadOnlyMemory<byte> [| 0x40uy; 0uy |])

/// <summary>A random anchor: formats 1 and 2 as plain coordinates, format 3 with devices, now and then none.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="allowNone">Whether the anchor may be none, a null or failing offset.</param>
/// <returns>The anchor.</returns>
let private randomAnchor (rnd: Random) (allowNone: bool) =
    let coordinate () = match rnd.Next 14 with | 0 -> 32767 | 1 -> -32768 | 2 -> 5 | _ -> rnd.Next(-300, 301)
    match rnd.Next 9 with
    | 0 when allowNone -> NoAnchor
    | 1 | 2 -> DeviceAnchor (coordinate (), coordinate (), (match rnd.Next 2 with | 0 -> hinting | _ -> NoDevice), (match rnd.Next 3 with | 0 -> hinting | _ -> NoDevice))
    | _ -> PlainAnchor (coordinate (), coordinate ())

/// <summary>A random subset of a set of glyphs, sorted.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
/// <returns>The subset, never empty.</returns>
let private subsetOf (rnd: Random) (alphabet: uint32 array) =
    let picked = alphabet |> Array.filter (fun _ -> rnd.Next 2 = 0)
    match picked.Length with
    | 0 -> [| alphabet[rnd.Next alphabet.Length] |]
    | _ -> picked

/// <summary>A direct table over a coverage: exact, or one that asks the structure.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="glyphs">The glyphs of the coverage.</param>
/// <returns>The direct table.</returns>
let private directOf (rnd: Random) (glyphs: uint32 array) =
    match rnd.Next 2 = 0 && glyphs.Length > 0 && Array.max glyphs < 4096u with
    | false -> structural
    | true ->
        { Table = ImmutableArray.CreateRange (Array.init (int (Array.max glyphs) + 1) (fun glyph -> match Array.IndexOf(glyphs, uint32 glyph) with | -1 -> 0xFFFFus | index -> uint16 index))
          Complete = true }

/// <summary>A random mark-to-base or mark-to-mark subtable over an alphabet.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
/// <returns>The subtable.</returns>
let private randomMarkAttachment (rnd: Random) (alphabet: uint32 array) =
    let marks = subsetOf rnd alphabet
    let targets = subsetOf rnd alphabet
    let classCount = match rnd.Next 12 with | 0 -> 0 | _ -> rnd.Next(1, 4)
    let records = match rnd.Next 6 with | 0 -> max 0 (marks.Length - 1) | 1 -> marks.Length + 1 | _ -> marks.Length
    let rows = match rnd.Next 6 with | 0 -> max 0 (targets.Length - 1) | 1 -> targets.Length + 1 | _ -> targets.Length
    { MarkCoverage = coverageOf marks
      MarkDirect = directOf rnd marks
      TargetCoverage = coverageOf targets
      TargetDirect = directOf rnd targets
      ClassCount = classCount
      MarkClasses = ImmutableArray.CreateRange (Array.init records (fun _ -> uint16 (rnd.Next(0, classCount + 2))))
      MarkAnchors = ImmutableArray.CreateRange (Array.init records (fun _ -> randomAnchor rnd false))
      Rows = rows
      Anchors = ImmutableArray.CreateRange (Array.init (rows * classCount) (fun _ -> randomAnchor rnd true)) }

/// <summary>A random mark-to-ligature subtable over an alphabet.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
/// <returns>The subtable.</returns>
let private randomMarkLigature (rnd: Random) (alphabet: uint32 array) =
    let marks = subsetOf rnd alphabet
    let ligatures = subsetOf rnd alphabet
    let classCount = match rnd.Next 12 with | 0 -> 0 | _ -> rnd.Next(1, 4)
    let records = match rnd.Next 6 with | 0 -> max 0 (marks.Length - 1) | 1 -> marks.Length + 1 | _ -> marks.Length
    let arrayLength = match rnd.Next 6 with | 0 -> max 0 (ligatures.Length - 1) | _ -> ligatures.Length
    let components = Array.init arrayLength (fun _ -> match rnd.Next 8 with | 0 -> 0 | _ -> rnd.Next(1, 4))
    let matrices = components |> Array.map (fun count -> Array.init (count * classCount) (fun _ -> randomAnchor rnd true))
    { LigMarkCoverage = coverageOf marks
      LigMarkDirect = directOf rnd marks
      LigatureCoverage = coverageOf ligatures
      LigatureDirect = directOf rnd ligatures
      LigClassCount = classCount
      LigMarkClasses = ImmutableArray.CreateRange (Array.init records (fun _ -> uint16 (rnd.Next(0, classCount + 2))))
      LigMarkAnchors = ImmutableArray.CreateRange (Array.init records (fun _ -> randomAnchor rnd false))
      Components = ImmutableArray.CreateRange components
      Starts = ImmutableArray.CreateRange (Array.scan (fun total (matrix: Anchor array) -> total + matrix.Length) 0 matrices)
      LigAnchors = ImmutableArray.CreateRange (Array.concat matrices) }

/// <summary>A random cursive attachment subtable over an alphabet.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
/// <returns>The subtable.</returns>
let private randomCursive (rnd: Random) (alphabet: uint32 array) =
    let glyphs = subsetOf rnd alphabet
    let records = match rnd.Next 6 with | 0 -> max 0 (glyphs.Length - 1) | _ -> glyphs.Length
    { CursiveCoverage = coverageOf glyphs
      CursiveDirect = directOf rnd glyphs
      Entries = ImmutableArray.CreateRange (Array.init records (fun _ -> randomAnchor rnd true))
      Exits = ImmutableArray.CreateRange (Array.init records (fun _ -> randomAnchor rnd true)) }

/// <summary>The lookup flags of a synthetic lookup: none, right to left, and those that skip glyph classes.</summary>
let private lookupFlags = [| 0u; 0u; 0u; 1u; 1u; 0x0002u; 0x0004u; 0x0008u; 0x0009u; 0x000Au; 0x0010u; 0x0100u; 0x0200u |]

/// <summary>A random lookup of one type, one to three subtables.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
/// <returns>The lookup.</returns>
let private randomLookup (rnd: Random) (alphabet: uint32 array) =
    let lookupType = rnd.Next(3, 7)
    let subtable () =
        match lookupType with
        | 3 -> CursivePos (randomCursive rnd alphabet)
        | 4 -> MarkBasePos (randomMarkAttachment rnd alphabet)
        | 5 -> MarkLigPos (randomMarkLigature rnd alphabet)
        | _ -> MarkMarkPos (randomMarkAttachment rnd alphabet)
    lookupOf lookupType lookupFlags[rnd.Next lookupFlags.Length] (List.init (rnd.Next(1, 4)) (fun _ -> subtable ()))

/// <summary>Builds a whole synthetic case from one seed: a table of attachment lookups, the map and stages that apply them,
/// a font of some scale and ppem, and a run whose glyphs the lookups meet often.</summary>
/// <param name="seed">The seed.</param>
/// <returns>The font, table, map, stages and run.</returns>
let private syntheticCase (seed: int) =
    let rnd = Random seed
    let alphabet = [| 1u .. uint32 (rnd.Next(3, 9)) |]
    let lookupCount = rnd.Next(1, 5)
    let table = tableOf (List.init lookupCount (fun _ -> randomLookup rnd alphabet))
    let maps =
        Array.init (rnd.Next(1, 7)) (fun _ ->
            { LookupIndex = rnd.Next lookupCount
              AutoZwnj = rnd.Next 3 = 0
              AutoZwj = rnd.Next 3 = 0
              Random = false
              PerSyllable = false
              Mask = [| 0x08u; 0x08u; 0x08u; 0x10u; 0x18u; 0u |][rnd.Next 6]
              FeatureTag = Tag.make 'm' 'a' 'r' 'k' })
    let stages =
        let cuts = Array.init (rnd.Next(1, 3)) (fun _ -> rnd.Next(0, maps.Length + 1)) |> Array.sort
        Array.append cuts [| maps.Length |] |> Array.map (fun last -> { LastLookup = last; Pause = PauseKind.NoPause })
    let length = match rnd.Next 8 with | 0 -> rnd.Next(0, 3) | 1 -> rnd.Next(30, 90) | _ -> rnd.Next(2, 14)
    let clusters = Array.scan (fun total _ -> total + uint32 (rnd.Next(0, 3))) 0u (Array.zeroCreate length) |> Array.truncate length
    let pool = Array.append alphabet (if rnd.Next 5 = 0 then [| 0u; 99u |] else [||])
    let infos =
        Array.init length (fun index ->
            { Codepoint = pool[rnd.Next pool.Length]
              Mask = (match rnd.Next 6 with | 0 -> 0x10u | 1 -> 0u | _ -> 0x08u)
              Cluster = (match rnd.Next 5 with | 0 -> uint32 index | _ -> clusters[index])
              Var1 = 0u
              Var2 = 0u })
    let props =
        Array.init length (fun _ ->
            let klass = [| GlyphPropsBits.Base; GlyphPropsBits.Mark; GlyphPropsBits.Mark; GlyphPropsBits.Ligature; 0us |][rnd.Next 5]
            let multiplied = match rnd.Next 8 with | 0 -> GlyphPropsBits.Multiplied | _ -> 0us
            let attachClass = match klass = GlyphPropsBits.Mark with | true -> uint16 (rnd.Next 3) <<< 8 | false -> 0us
            { Unchecked.defaultof<GlyphProps> with
                UnicodeProps = (match rnd.Next 10 with | 0 -> 0x20us | _ -> 0us)
                GlyphClass = klass ||| multiplied ||| attachClass
                LigProps = (match rnd.Next 3 with | 0 -> 0uy | _ -> byte (((rnd.Next 3) <<< 5) ||| (match rnd.Next 3 with | 0 -> 0x10 | _ -> 0) ||| rnd.Next 4)) })
    let positions =
        Array.init length (fun index ->
            { XAdvance = (match rnd.Next 12 with | 0 -> Int32.MaxValue - rnd.Next 40 | 1 -> Int32.MinValue + rnd.Next 40 | _ -> 500 + index)
              YAdvance = 0
              XOffset = rnd.Next(-3, 4)
              YOffset = rnd.Next(-3, 4)
              Var = (match rnd.Next 4 with | 0 -> 0u | 1 -> uint32 (rnd.Next 4) | 2 -> (uint32 (rnd.Next 3) <<< 16) ||| uint32 (uint16 (rnd.Next(-3, 4))) | _ -> 0u) })
    let direction = [| Direction.Ltr; Direction.Ltr; Direction.Rtl; Direction.Rtl; Direction.Ttb; Direction.Btt; Direction.Invalid |][rnd.Next 7]
    let run =
        { runOf infos props positions direction with
            HavePositions = rnd.Next 30 <> 0
            Flags = LanguagePrimitives.EnumOfValue<uint32, BufferFlags> (match rnd.Next 3 with | 0 -> 0u | 1 -> 64u | _ -> uint32 (rnd.Next 0x100))
            ClusterLevel = [| ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes |][rnd.Next 4]
            Successful = rnd.Next 40 <> 0
            ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 (rnd.Next 0x100))
            MaxOps = (match rnd.Next 8 with | 0 -> rnd.Next(0, 12) | 1 -> rnd.Next(0, 400) | _ -> GlyphRun.MaxOpsDefault) }
    let scale = [| 1000; 1000; 2048; 17; 4096 |][rnd.Next 5]
    let ppem = [| 0u; 0u; 12u; 100u |][rnd.Next 4]
    struct (fontOf scale ppem, table, ImmutableArray.CreateRange maps, ImmutableArray.CreateRange stages, run)

/// <summary>What one synthetic case did, for the report.</summary>
[<Struct>]
type private Outcome =
    { Attached: bool
      Cursive: bool
      Overflowed: bool }

/// <summary>Checks one synthetic case and, when it fails, prints the seed that replays it.</summary>
/// <param name="seed">The seed.</param>
/// <returns>What the case did.</returns>
let private checkSynthetic (seed: int) =
    try
        let struct (font, table, maps, stages, run) = syntheticCase seed
        let kernel = checkAgainstReference $"synthetic seed {seed}" font table maps stages run
        { Attached = kernel.Positions.AsSpan().ToArray() <> run.Positions.AsSpan().ToArray()
          Cursive = kernel.Positions.AsSpan().ToArray() |> Array.exists (fun position -> (position.Var >>> 16) &&& 0xFFu = 2u)
          Overflowed = false }
    with error ->
        printfn $"FAILED synthetic seed {seed} {error.Message}"
        reraise ()

/// <summary>The GPOS attachment lookups, held to a literal transliteration of upstream and to their own sanitizer.</summary>
[<Tests>]
let tests =
    testList "P5.S11" [
        test "P5.S11 mark-to-base bytes parse into the coverages, the mark array and the anchor matrix" {
            let table = loadGpos (gposOf 4 0 (markBaseBytes ()))
            Expect.equal table.Lookups.Length 1 "the table is accepted"
            let subtable = table.Lookups[0].Value.Subtables[0].Subtable
            match subtable with
            | MarkBasePos attachment ->
                Expect.sequenceEqual (Coverage.glyphs attachment.MarkCoverage |> Seq.toArray) [| 5u; 6u |] "mark coverage"
                Expect.sequenceEqual (Coverage.glyphs attachment.TargetCoverage |> Seq.toArray) [| 1u; 2u |] "base coverage"
                Expect.equal attachment.ClassCount 2 "class count"
                Expect.sequenceEqual (attachment.MarkClasses.AsSpan().ToArray()) [| 0us; 1us |] "mark classes"
                Expect.sequenceEqual (attachment.MarkAnchors.AsSpan().ToArray()) [| PlainAnchor (10, 20); PlainAnchor (30, 40) |] "mark anchors"
                Expect.equal attachment.Rows 2 "rows"
                Expect.sequenceEqual (attachment.Anchors.AsSpan().ToArray()) [| PlainAnchor (110, 120); PlainAnchor (130, 140); PlainAnchor (210, 220); PlainAnchor (230, 240) |] "the matrix, row by row"
            | _ -> failtest "a mark-to-base subtable"
        }

        test "P5.S11 anchors of format 2 and 3 decode, and a device that fails its check makes the anchor none" {
            let withMatrix (first: byte array) (second: byte array) =
                let markCoverage = coverageBytes [ 5; 6 ]
                let baseCoverage = coverageBytes [ 1 ]
                let markArray = Array.concat [ u16 2; u16 0; u16 10; u16 0; u16 10; anchor1 1 2 ]
                let matrix = Array.concat [ u16 1; u16 6; u16 (6 + first.Length); first; second ]
                let markCoverageAt = 12
                let baseCoverageAt = markCoverageAt + markCoverage.Length
                let markArrayAt = baseCoverageAt + baseCoverage.Length
                let baseArrayAt = markArrayAt + markArray.Length
                Array.concat [ u16 1; u16 markCoverageAt; u16 baseCoverageAt; u16 2; u16 markArrayAt; u16 baseArrayAt; markCoverage; baseCoverage; markArray; matrix ]
            let table = loadGpos (gposOf 4 0 (withMatrix (anchor2 7 8 3) (anchor3 9 10)))
            Expect.equal table.Lookups.Length 1 "the table is accepted"
            match table.Lookups[0].Value.Subtables[0].Subtable with
            | MarkBasePos attachment ->
                Expect.equal attachment.Anchors[0] (PlainAnchor (7, 8)) "format 2 reads as its coordinates: the OpenType font funcs give no contour point"
                match attachment.Anchors[1] with
                | DeviceAnchor (9, 10, HintingDevice (11us, 13us, 1us, _), VariationIndex (0us, 0us)) -> ()
                | other -> failtestf "a format 3 anchor with its two devices, got %A" other
            | _ -> failtest "a mark-to-base subtable"
            let broken = anchor3 9 10
            broken[6] <- 0xFFuy
            broken[7] <- 0xF0uy
            let lookups = (loadGpos (gposOf 4 0 (withMatrix (anchor2 7 8 3) broken))).Lookups
            Expect.equal lookups.Length 1 "the table is still accepted: the anchors of a matrix are checked lazily"
            match lookups[0].Value.Subtables[0].Subtable with
            | MarkBasePos attachment -> Expect.equal attachment.Anchors[1] NoAnchor "a device offset beyond the table fails the anchor's check at apply, so the anchor is not found"
            | _ -> failtest "a mark-to-base subtable"
        }

        test "P5.S11 the sanitizer of each attachment type rejects what HarfBuzz rejects and accepts the unchanged table" {
            let ligature =
                let markCoverage = coverageBytes [ 5 ]
                let ligatureCoverage = coverageBytes [ 1 ]
                let markArray = Array.concat [ u16 1; u16 0; u16 6; anchor1 1 2 ]
                let attach = Array.concat [ u16 1; u16 4; anchor1 3 4 ]
                let ligatureArray = Array.concat [ u16 1; u16 4; attach ]
                let markCoverageAt = 12
                let ligatureCoverageAt = markCoverageAt + markCoverage.Length
                let markArrayAt = ligatureCoverageAt + ligatureCoverage.Length
                let ligatureArrayAt = markArrayAt + markArray.Length
                Array.concat [ u16 1; u16 markCoverageAt; u16 ligatureCoverageAt; u16 1; u16 markArrayAt; u16 ligatureArrayAt; markCoverage; ligatureCoverage; markArray; ligatureArray ]
            let cursive =
                let coverage = coverageBytes [ 3; 4 ]
                let entry = anchor1 5 6
                let header = 6 + 2 * 4
                Array.concat [ u16 1; u16 header; u16 2; u16 (header + coverage.Length); u16 0; u16 0; u16 (header + coverage.Length); coverage; entry ]
            let accepted (type_: int) (bytes: byte array) = (loadGpos (gposOf type_ 0 bytes)).Lookups.Length = 1
            let changed (bytes: byte array) (at: int) (value: int) =
                let copy = Array.copy bytes
                copy[at] <- byte (value >>> 8)
                copy[at + 1] <- byte value
                copy
            Expect.isTrue (accepted 4 (markBaseBytes ())) "mark-to-base"
            Expect.isTrue (accepted 5 ligature) "mark-to-ligature"
            Expect.isTrue (accepted 3 cursive) "cursive"
            Expect.isTrue (accepted 6 (markBaseBytes ())) "mark-to-mark has the layout of mark-to-base, with 16-bit rows"
            let markArrayAt = int (markBaseBytes ()).[8] * 256 + int (markBaseBytes ()).[9]
            for (name, kind, bytes) in [ "mark-to-base", 4, markBaseBytes (); "mark-to-ligature", 5, ligature; "cursive", 3, cursive ] do
                Expect.isTrue (accepted kind (Array.take (bytes.Length - 1) bytes)) $"{name}: a truncated last anchor is accepted, the anchors of a matrix and of a cursive record are checked when they are applied"
                Expect.isFalse (accepted kind (changed bytes 2 0xFFFF)) $"{name}: the first coverage offset past the table makes it fail"
                Expect.isFalse (accepted kind (Array.take 8 bytes)) $"{name}: a table cut inside its header makes it fail"
            for kind in [ 4; 6 ] do
                let bytes = markBaseBytes ()
                Expect.isFalse (accepted kind (changed bytes markArrayAt 0xFFFF)) "a mark array that claims 65535 records fails"
                Expect.isFalse (accepted kind (changed bytes (markArrayAt + 4) 0xFFF0)) "a mark record whose anchor offset is past the table fails: mark anchors are checked eagerly"
                Expect.isTrue (accepted kind (changed bytes (markArrayAt + 4) 0)) "a null mark anchor is the Null anchor and passes"
                Expect.isFalse (accepted kind (changed bytes 6 0xFFFF)) "a class count whose matrix does not fit makes it fail"
        }

        test "P5.S11 roundf takes half towards positive infinity, not away from zero: -2.5 gives -2 and 2.5 gives 3" {
            let font = fontOf 500 0u
            let env = envOf font Direction.Ltr 0u
            let subtable = markAttachmentOf [| 5u |] [| 1u |] 1 [| 0, PlainAnchor (0, 0) |] 1 [| PlainAnchor (5, -5) |]
            let infos = infosOf [| 1u; 5u |]
            let props = propsOf [| GlyphPropsBits.Base; GlyphPropsBits.Mark |]
            let outcome = GposAttach.applySubtable env (MarkBasePos subtable) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan (positionsOf 2)) 1 -1 0
            Expect.isTrue outcome.Applied "the mark attaches"
            Expect.equal outcome.Position.XOffset 3 "base 5 at half scale is 2.5 and the mark at the origin: 2.5 rounds to 3"
            Expect.equal outcome.Position.YOffset -2 "base -5 at half scale is -2.5: it rounds to -2, where C's round would give -3"
        }

        test "P5.S11 last_base is shared across the subtables of a lookup, as upstream does" {
            let font = fontOf 1000 0u
            let env = envOf font Direction.Ltr 0u
            let anchors = [| PlainAnchor (1, 1) |]
            let subtableA = markAttachmentOf [| 6u |] [| 2u |] 1 [| 0, PlainAnchor (0, 0) |] 1 anchors
            let subtableB = markAttachmentOf [| 7u |] [| 1u |] 1 [| 0, PlainAnchor (0, 0) |] 1 anchors
            let infos = infosOf [| 1u; 2u; 6u; 7u |]
            let multiplied = GlyphPropsBits.Base ||| GlyphPropsBits.Multiplied
            let props =
                [| { Unchecked.defaultof<GlyphProps> with GlyphClass = multiplied; LigProps = 0uy }
                   { Unchecked.defaultof<GlyphProps> with GlyphClass = multiplied; LigProps = 1uy }
                   { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Mark }
                   { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Mark } |]
            let positions = positionsOf 4
            let first = GposAttach.applySubtable env (MarkBasePos subtableA) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan positions) 2 -1 0
            Expect.isTrue first.Applied "the first subtable attaches the first mark to the glyph it covers, the second of a multiple substitution"
            Expect.equal first.LastBase 1 "and caches it"
            Expect.equal first.LastBaseUntil 2 "as good until the mark's index"
            let shared = GposAttach.applySubtable env (MarkBasePos subtableB) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan positions) 3 first.LastBase first.LastBaseUntil
            Expect.isFalse shared.Applied "the second subtable, given the cache, takes the cached base, which it does not cover"
            Expect.equal shared.ConcatFrom 1 "and records the miss from that base"
            let fresh = GposAttach.applySubtable env (MarkBasePos subtableB) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan positions) 3 -1 0
            Expect.isTrue fresh.Applied "with a cache of its own it would skip the glyph it does not accept and attach to the first glyph"
            Expect.equal fresh.BreakFrom 0 "the unsafe-to-break range starts at that glyph"
        }

        test "P5.S11 a mark attaches to the ligature component its ligature id and component name, else to the last" {
            let font = fontOf 1000 0u
            let env = envOf font Direction.Ltr 0u
            let matrix = [| PlainAnchor (100, 0); PlainAnchor (200, 0); PlainAnchor (300, 0) |]
            let subtable =
                { LigMarkCoverage = coverageOf [| 5u |]
                  LigMarkDirect = structural
                  LigatureCoverage = coverageOf [| 1u |]
                  LigatureDirect = structural
                  LigClassCount = 1
                  LigMarkClasses = ImmutableArray.Create 0us
                  LigMarkAnchors = ImmutableArray.Create (PlainAnchor (0, 0))
                  Components = ImmutableArray.Create 3
                  Starts = ImmutableArray.CreateRange [ 0; 3 ]
                  LigAnchors = ImmutableArray.CreateRange matrix }
            let infos = infosOf [| 1u; 5u |]
            let ligatureProps = { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Ligature; LigProps = byte ((3 <<< 5) ||| 0x10 ||| 3) }
            let markAt (comp: int) (ligId: int) =
                let props = [| ligatureProps; { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Mark; LigProps = byte ((ligId <<< 5) ||| comp) } |]
                (GposAttach.applySubtable env (MarkLigPos subtable) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan (positionsOf 2)) 1 -1 0).Position.XOffset
            Expect.equal (markAt 1 3) 100 "the mark of the same ligature, component 1: the offset is the base anchor minus the mark anchor"
            Expect.equal (markAt 2 3) 200 "component 2"
            Expect.equal (markAt 9 3) 300 "a component beyond the last is clamped to the last"
            Expect.equal (markAt 2 2) 300 "a mark of another ligature takes the last component"
            Expect.equal (markAt 0 3) 300 "a mark of component zero takes the last component"
        }

        test "P5.S11 a mark-to-mark mark attaches only to a mark of the same base or component, and one that is a ligature itself matches" {
            let font = fontOf 1000 0u
            let env = envOf font Direction.Ltr 0u
            let subtable = markAttachmentOf [| 6u |] [| 5u |] 1 [| 0, PlainAnchor (0, 0) |] 1 [| PlainAnchor (50, 60) |]
            let infos = infosOf [| 1u; 5u; 6u |]
            let mark (id: int) (comp: int) = { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Mark; LigProps = byte ((id <<< 5) ||| comp) }
            let applies (first: GlyphProps) (second: GlyphProps) =
                let props = [| { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Base }; first; second |]
                (GposAttach.applySubtable env (MarkMarkPos subtable) (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan (positionsOf 3)) 2 -1 0).Applied
            Expect.isTrue (applies (mark 0 0) (mark 0 0)) "both marks of one base"
            Expect.isTrue (applies (mark 2 1) (mark 2 1)) "the same ligature component"
            Expect.isFalse (applies (mark 2 1) (mark 2 2)) "other components of one ligature"
            Expect.isFalse (applies (mark 2 1) (mark 3 1)) "components of other ligatures"
            Expect.isTrue (applies (mark 2 0) (mark 3 1)) "the earlier mark is a ligature itself: component zero of a ligature id"
            Expect.isTrue (applies (mark 2 1) (mark 3 0)) "the later mark is a ligature itself"
        }

        test "P5.S11 a cursive attachment swaps child and parent unless the lookup is right to left, and reverses an old chain" {
            let font = fontOf 1000 0u
            let cursive =
                { CursiveCoverage = coverageOf [| 1u; 2u |]
                  CursiveDirect = structural
                  Entries = ImmutableArray.CreateRange [ PlainAnchor (0, 10); PlainAnchor (0, 10) ]
                  Exits = ImmutableArray.CreateRange [ PlainAnchor (400, 30); PlainAnchor (400, 30) ] }
            let table flags = tableOf [ lookupOf 3 flags [ CursivePos cursive ] ]
            let run direction =
                runOf (infosOf [| 1u; 2u; 1u |]) (propsOf [| GlyphPropsBits.Base; GlyphPropsBits.Base; GlyphPropsBits.Base |]) (positionsOf 3) direction
            let leftToRight = position font (table 0u) (run Direction.Ltr)
            let positions = leftToRight.Positions.AsSpan().ToArray()
            Expect.equal positions[1].YOffset 20 "without the right-to-left flag the later glyph is the child and takes the difference of the anchors, negated: 30 - 10"
            Expect.equal (positions[1].Var >>> 16 &&& 0xFFu) 2u "its type is cursive"
            Expect.equal (int (int16 (positions[1].Var &&& 0xFFFFu))) -1 "and it attaches to the glyph before it"
            Expect.equal (positions[2].Var &&& 0xFFFFu) 0xFFFFu "the third glyph attaches to the second"
            Expect.isTrue (leftToRight.ScratchFlags &&& ScratchFlags.HasGposAttachment <> ScratchFlags.Default) "the run has a GPOS attachment"
            let rightToLeft = position font (table 1u) (run Direction.Rtl)
            let rtl = rightToLeft.Positions.AsSpan().ToArray()
            Expect.equal (int (int16 (rtl[0].Var &&& 0xFFFFu))) 1 "with the flag the earlier glyph is the child, attached forward"
            Expect.equal rtl[0].YOffset -20 "and takes the difference of the anchors as it is: 10 - 30"
            Expect.equal (int (int16 (rtl[1].Var &&& 0xFFFFu))) 1 "the second glyph attaches to the third and the first chain is reversed onto it, not left pointing back"
        }

        test "P5.S11 position_finish_offsets propagates in place in the order of the direction: a cycle of cursive links ends differently going each way" {
            let font = fontOf 1000 0u
            let cursive = 0x02u <<< 16
            let positions =
                [| { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 5; Var = cursive ||| 1u }
                   { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 7; Var = cursive ||| 0xFFFFu } |]
            let finish direction =
                let run = { runOf (infosOf [| 1u; 1u |]) (propsOf [| GlyphPropsBits.Base; GlyphPropsBits.Base |]) positions direction with ScratchFlags = ScratchFlags.HasGposAttachment }
                (GposApply.finishOffsets font run).Positions.AsSpan().ToArray() |> Array.map (fun position -> position.YOffset, position.Var &&& 0xFFFFu)
            Expect.equal (finish Direction.Ltr) [| 17, 0u; 12, 0u |] "forwards: glyph 0 is started first, glyph 1 takes glyph 0's offset, then glyph 0 takes glyph 1's new offset: 7 + 5 = 12 and 5 + 12 = 17"
            Expect.equal (finish Direction.Rtl) [| 12, 0u; 19, 0u |] "backwards: glyph 1 is started first: 5 + 7 = 12 for glyph 0, then 7 + 12 = 19 for glyph 1"
        }

        test "P5.S11 a lookup type no table dispatches keeps its subtable offsets unchecked, as HarfBuzz does" {
            let table =
                let lookup = Array.concat [ u16 0x0500; u16 0x0100; u16 1; u16 0xFFF0 ]
                let lookupList = Array.concat [ u16 1; u16 4; lookup ]
                Array.concat [ u32 0x00010000; u16 10; u16 12; u16 14; u16 0; u16 0; lookupList ]
            Expect.equal (loadGpos table).Lookups.Length 1 "a lookup of an unknown type with a subtable offset past the table is accepted"
            let known = Array.copy table
            known[19] <- 0x04uy
            known[18] <- 0x00uy
            Expect.equal (loadGpos known).Lookups.Length 0 "the same lookup as a mark-to-base lookup is rejected"
        }

        test "P5.S11 the kernel equals a literal transliteration of upstream in every field over random attachment tables" {
            let seeds = Gen.sampleWithSeed (Rnd Seed) 30 SyntheticCaseCount (Gen.choose (0, Int32.MaxValue))
            let results = seeds |> Seq.map checkSynthetic |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"P5.S11 synthetic property: {results.Length} cases, {count (fun outcome -> outcome.Attached)} changed a position, {count (fun outcome -> outcome.Cursive)} ended with a cursive link"
            Expect.isGreaterThan (count (fun outcome -> outcome.Attached)) 2000 "many cases move a glyph"
            Expect.isGreaterThan (count (fun outcome -> outcome.Cursive)) 300 "many cases have a cursive link"
        }

        test "P5.S11 the edges: an empty run, one glyph, a mark first, a mark last, no base, and a nesting of cursive chains" {
            let font = fontOf 1000 0u
            let attachment = markAttachmentOf [| 5u |] [| 1u |] 1 [| 0, PlainAnchor (10, 10) |] 1 [| PlainAnchor (30, 40) |]
            let table = tableOf [ lookupOf 4 0u [ MarkBasePos attachment ] ]
            let struct (maps, stages) = mapOf 1
            let check (what: string) (glyphs: uint32 array) (classes: uint16 array) =
                let run = runOf (infosOf glyphs) (propsOf classes) (positionsOf glyphs.Length) Direction.Ltr
                checkAgainstReference what font table maps stages run
            let empty = check "empty" [||] [||]
            Expect.equal empty.Infos.Length 0 "an empty run stays empty"
            Expect.sequenceEqual ((check "one mark" [| 5u |] [| GlyphPropsBits.Mark |]).Positions.AsSpan().ToArray()) (positionsOf 1) "a lone mark has no base and stays where it is"
            let attached = check "base then mark, the mark last" [| 1u; 5u |] [| GlyphPropsBits.Base; GlyphPropsBits.Mark |]
            Expect.equal attached.Positions[1].XOffset 20 "the mark last attaches to the base before it"
            Expect.equal attached.Positions[1].YOffset 30 "with the difference of the anchors"
            let first = check "mark then base" [| 5u; 1u |] [| GlyphPropsBits.Mark; GlyphPropsBits.Base |]
            Expect.equal first.Positions[0].XOffset 0 "a mark before its base does not attach"
            let chain = tableOf [ lookupOf 3 1u [ CursivePos { CursiveCoverage = coverageOf [| 1u |]; CursiveDirect = structural; Entries = ImmutableArray.Create (PlainAnchor (0, 1)); Exits = ImmutableArray.Create (PlainAnchor (500, 2)) } ] ]
            let long = Array.create 200 1u
            let run = runOf (infosOf long) (propsOf (Array.create 200 GlyphPropsBits.Base)) (positionsOf 200) Direction.Ltr
            let struct (cursiveMaps, cursiveStages) = mapOf 1
            checkAgainstReference "a chain of 200 cursive glyphs" font chain cursiveMaps cursiveStages run |> ignore
        }

        test "P5.S11 the in-place pair loop refuses a run that a mark lookup can touch, and a pair lookup alone is still applied in place" {
            let font = fontOf 1000 0u
            let attachment = markAttachmentOf [| 5u |] [| 1u |] 1 [| 0, PlainAnchor (10, 10) |] 1 [| PlainAnchor (30, 40) |]
            let pair =
                PairPosGlyphs (coverageOf [| 1u |], 4us, 0us, ImmutableArray.CreateRange [ 0; 1 ], ImmutableArray.Create 1u, ImmutableArray.Create 25s, ImmutableArray<Device>.Empty, Digest.full, { Table = ImmutableArray.CreateRange [ 0xFFFFus; 0us ]; Complete = true })
            let table = tableOf [ lookupOf 2 0u [ pair ]; lookupOf 4 0u [ MarkBasePos attachment ] ]
            let struct (maps, stages) = mapOf 2
            let gdef = font.Ot.Gdef.Value
            let covered = Engine.gposLookupCoveredOf table maps
            let glyphLookups = Engine.gposGlyphLookupsOf table maps
            let pairs = Engine.gposPairsOf table gdef maps stages 1u covered glyphLookups
            let own (glyphs: uint32 array) =
                let infos = ImmutableArray.CreateRange (infosOf glyphs)
                let positions = ImmutableArray.CreateRange (positionsOf glyphs.Length)
                Engine.positionOwned pairs font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault
            Expect.isTrue (own [| 1u; 1u |] |> ValueOption.isSome) "a run of the pair's glyphs alone is applied in place"
            Expect.isTrue (own [| 1u; 5u |] |> ValueOption.isNone) "a run with a glyph the mark lookup starts at is refused: the lean loop cannot attach"
        }

        test "P5.S11 a mark lookup on a run with no glyph it covers costs nothing, and a covered run where nothing attaches allocates nothing" {
            let font = fontOf 1000 0u
            let attachment = markAttachmentOf [| 5u |] [| 1u |] 1 [| 0, PlainAnchor (10, 10) |] 1 [| PlainAnchor (30, 40) |]
            let table = tableOf [ lookupOf 4 0u [ MarkBasePos attachment ] ]
            let struct (maps, stages) = mapOf 1
            let gdef = font.Ot.Gdef.Value
            let covered = Engine.gposLookupCoveredOf table maps
            let glyphLookups = Engine.gposGlyphLookupsOf table maps
            let big (glyph: uint32) (klass: uint16) = runOf (infosOf (Array.create 10000 glyph)) (propsOf (Array.create 10000 klass)) (positionsOf 10000) Direction.Ltr
            let uncovered = big 2u GlyphPropsBits.Base
            let marksOnly = big 5u GlyphPropsBits.Mark
            let gated = Allocation.bytesAllocatedAtBest (fun () -> Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty uncovered |> ignore |> TailCalls.settle)
            Expect.equal gated 0L "no glyph of the run is covered: the lookup is not visited and the run comes back as it went in"
            Expect.isTrue (obj.ReferenceEquals(Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty uncovered, uncovered)) "the same instance"
            let missed = Allocation.bytesAllocatedAtBest (fun () -> Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty marksOnly |> ignore |> TailCalls.settle)
            Expect.equal missed 0L "10000 marks and no base: every mark is searched for a base and none attaches, with nothing allocated"
            Expect.isTrue (obj.ReferenceEquals(Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty marksOnly, marksOnly)) "and the run is returned as it went in"
        }
    ]
