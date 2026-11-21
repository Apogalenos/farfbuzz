module Farfbuzz.Tests.Layout.GposContextTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The number of random tables of the property.</summary>
[<Literal>]
let private PropertyCaseCount = 6000

/// <summary>A big-endian unsigned 16-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u16 (value: int) = [| byte (value >>> 8); byte value |]

/// <summary>A big-endian unsigned 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u32 (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A coverage of format 1 over sorted glyphs.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The bytes.</returns>
let private coverageBytes (glyphs: int list) =
    Array.concat [ u16 1; u16 glyphs.Length; Array.concat (glyphs |> List.map u16) ]

/// <summary>A class definition of format 2 over sorted glyph ranges of one glyph each.</summary>
/// <param name="classes">The glyph and class of each classified glyph, sorted by glyph.</param>
/// <returns>The bytes.</returns>
let private classBytes (classes: (int * int) list) =
    Array.concat [ u16 2; u16 classes.Length; Array.concat (classes |> List.map (fun (glyph, klass) -> Array.concat [ u16 glyph; u16 glyph; u16 klass ])) ]

/// <summary>The offsets at which blocks laid out one after the other start.</summary>
/// <param name="header">The offset of the first block.</param>
/// <param name="blocks">The blocks.</param>
/// <returns>The offsets, one for each block.</returns>
let private offsetsOf (header: int) (blocks: byte array list) =
    blocks |> List.scan (fun at block -> at + block.Length) header |> List.take blocks.Length

/// <summary>The lookup records of a rule: the sequence index and lookup index of each.</summary>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private recordBytes (records: (int * int) list) =
    Array.concat (records |> List.map (fun (sequence, lookup) -> Array.append (u16 sequence) (u16 lookup)))

/// <summary>A rule set: the offsets of its rules and the rules.</summary>
/// <param name="rules">The rules.</param>
/// <returns>The bytes.</returns>
let private ruleSetBytes (rules: byte array list) =
    Array.concat [ u16 rules.Length; Array.concat (offsetsOf (2 + 2 * rules.Length) rules |> List.map u16); Array.concat rules ]

/// <summary>A context rule: the input after the first glyph and the records.</summary>
/// <param name="input">The input values after the first.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private ruleBytes (input: int list) (records: (int * int) list) =
    Array.concat [ u16 (input.Length + 1); u16 records.Length; Array.concat (input |> List.map u16); recordBytes records ]

/// <summary>A chained rule: the backtrack, the input after the first glyph, the lookahead and the records.</summary>
/// <param name="backtrack">The backtrack values, nearest first.</param>
/// <param name="input">The input values after the first.</param>
/// <param name="lookahead">The lookahead values.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private chainRuleBytes (backtrack: int list) (input: int list) (lookahead: int list) (records: (int * int) list) =
    Array.concat [ u16 backtrack.Length; Array.concat (backtrack |> List.map u16); u16 (input.Length + 1); Array.concat (input |> List.map u16); u16 lookahead.Length; Array.concat (lookahead |> List.map u16); u16 records.Length; recordBytes records ]

/// <summary>A context subtable of the first format.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="sets">The rule set of each covered glyph.</param>
/// <returns>The bytes.</returns>
let private context1 (covered: int list) (sets: byte array list) =
    let coverage = coverageBytes covered
    let header = 6 + 2 * sets.Length
    Array.concat [ u16 1; u16 header; u16 sets.Length; Array.concat (offsetsOf (header + coverage.Length) sets |> List.map u16); coverage; Array.concat sets ]

/// <summary>A context subtable of the second format.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="classes">The class of each classified glyph.</param>
/// <param name="sets">The rule set of each class, from class zero.</param>
/// <returns>The bytes.</returns>
let private context2 (covered: int list) (classes: (int * int) list) (sets: byte array list) =
    let coverage = coverageBytes covered
    let classDef = classBytes classes
    let header = 8 + 2 * sets.Length
    let after = header + coverage.Length + classDef.Length
    Array.concat [ u16 2; u16 header; u16 (header + coverage.Length); u16 sets.Length; Array.concat (offsetsOf after sets |> List.map u16); coverage; classDef; Array.concat sets ]

/// <summary>A context subtable of the third format.</summary>
/// <param name="coverages">The glyphs each input position covers.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private context3 (coverages: int list list) (records: (int * int) list) =
    let bodies = coverages |> List.map coverageBytes
    let header = 6 + 2 * bodies.Length + 4 * records.Length
    Array.concat [ u16 3; u16 bodies.Length; u16 records.Length; Array.concat (offsetsOf header bodies |> List.map u16); recordBytes records; Array.concat bodies ]

/// <summary>A chained-context subtable of the first format.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="sets">The rule set of each covered glyph.</param>
/// <returns>The bytes.</returns>
let private chain1 (covered: int list) (sets: byte array list) =
    let coverage = coverageBytes covered
    let header = 6 + 2 * sets.Length
    Array.concat [ u16 1; u16 header; u16 sets.Length; Array.concat (offsetsOf (header + coverage.Length) sets |> List.map u16); coverage; Array.concat sets ]

/// <summary>A chained-context subtable of the second format.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="backtrackClasses">The backtrack class of each classified glyph.</param>
/// <param name="inputClasses">The input class of each classified glyph.</param>
/// <param name="lookaheadClasses">The lookahead class of each classified glyph.</param>
/// <param name="sets">The rule set of each input class, from class zero.</param>
/// <returns>The bytes.</returns>
let private chain2 (covered: int list) (backtrackClasses: (int * int) list) (inputClasses: (int * int) list) (lookaheadClasses: (int * int) list) (sets: byte array list) =
    let coverage = coverageBytes covered
    let defs = [ classBytes backtrackClasses; classBytes inputClasses; classBytes lookaheadClasses ]
    let header = 12 + 2 * sets.Length
    let defOffsets = offsetsOf (header + coverage.Length) defs
    let after = header + coverage.Length + (defs |> List.sumBy (fun bytes -> bytes.Length))
    Array.concat [ u16 2; u16 header; Array.concat (defOffsets |> List.map u16); u16 sets.Length; Array.concat (offsetsOf after sets |> List.map u16); coverage; Array.concat defs; Array.concat sets ]

/// <summary>A chained-context subtable of the third format.</summary>
/// <param name="backtrack">The glyphs each backtrack position covers, nearest first.</param>
/// <param name="input">The glyphs each input position covers.</param>
/// <param name="lookahead">The glyphs each lookahead position covers.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private chain3 (backtrack: int list list) (input: int list list) (lookahead: int list list) (records: (int * int) list) =
    let bodies = (backtrack @ input @ lookahead) |> List.map coverageBytes
    let header = 2 + 2 + 2 * backtrack.Length + 2 + 2 * input.Length + 2 + 2 * lookahead.Length + 2 + 4 * records.Length
    let offsets = offsetsOf header bodies |> List.map u16
    Array.concat [ u16 3; u16 backtrack.Length; Array.concat (offsets |> List.take backtrack.Length); u16 input.Length; Array.concat (offsets |> List.skip backtrack.Length |> List.take input.Length); u16 lookahead.Length; Array.concat (offsets |> List.skip (backtrack.Length + input.Length)); u16 records.Length; recordBytes records; Array.concat bodies ]

/// <summary>A single adjustment subtable that adds a value to the horizontal advance of the covered glyphs, format 1.</summary>
/// <param name="glyphs">The covered glyphs.</param>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private singleBytes (glyphs: int list) (value: int) =
    Array.concat [ u16 1; u16 8; u16 4; u16 (value &&& 0xFFFF); coverageBytes glyphs ]

/// <summary>A pair adjustment subtable of the first format with one pair whose first glyph's advance changes.</summary>
/// <param name="first">The first glyph.</param>
/// <param name="second">The second glyph.</param>
/// <param name="value">The advance added to the first glyph.</param>
/// <returns>The bytes.</returns>
let private pairBytes (first: int) (second: int) (value: int) =
    Array.concat [ u16 1; u16 12; u16 4; u16 0; u16 1; u16 18; coverageBytes [ first ]; u16 1; u16 second; u16 (value &&& 0xFFFF) ]

/// <summary>A mark-to-base subtable of format 1: marks 5 and 6 of classes 0 and 1 over bases 1 and 2, with the anchors
/// (10, 20) and (30, 40) for the marks and (110, 120), (130, 140), (210, 220) and (230, 240) for the bases.</summary>
/// <returns>The bytes.</returns>
let private markBaseBytes () =
    let anchor (x: int) (y: int) = Array.concat [ u16 1; u16 x; u16 y ]
    let markCoverage = coverageBytes [ 5; 6 ]
    let baseCoverage = coverageBytes [ 1; 2 ]
    let markAnchors = [ anchor 10 20; anchor 30 40 ]
    let markArray =
        let head = 2 + 2 * 4
        Array.concat [ u16 2; u16 0; u16 head; u16 1; u16 (head + markAnchors[0].Length); markAnchors[0]; markAnchors[1] ]
    let baseAnchors = [ anchor 110 120; anchor 130 140; anchor 210 220; anchor 230 240 ]
    let baseArray =
        let head = 2 + 4 * 2
        Array.concat [ u16 2; Array.concat (offsetsOf head baseAnchors |> List.map u16); Array.concat baseAnchors ]
    let header = 12
    let baseCoverageAt = header + markCoverage.Length
    let markArrayAt = baseCoverageAt + baseCoverage.Length
    let baseArrayAt = markArrayAt + markArray.Length
    Array.concat [ u16 1; u16 header; u16 baseCoverageAt; u16 2; u16 markArrayAt; u16 baseArrayAt; markCoverage; baseCoverage; markArray; baseArray ]

/// <summary>A lookup of subtables.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="flag">The lookup flag.</param>
/// <param name="subtables">The subtables.</param>
/// <returns>The bytes.</returns>
let private lookupBytes (lookupType: int) (flag: int) (subtables: byte array list) =
    let header = 6 + 2 * subtables.Length
    Array.concat [ u16 lookupType; u16 flag; u16 subtables.Length; Array.concat (offsetsOf header subtables |> List.map u16); Array.concat subtables ]

/// <summary>An extension subtable that wraps a subtable of a type.</summary>
/// <param name="innerType">The wrapped type.</param>
/// <param name="subtable">The wrapped subtable.</param>
/// <returns>The bytes.</returns>
let private extension (innerType: int) (subtable: byte array) =
    Array.concat [ u16 1; u16 innerType; u32 8; subtable ]

/// <summary>A GPOS table of lookups, with no scripts or features.</summary>
/// <param name="lookups">The lookups.</param>
/// <returns>The bytes.</returns>
let private tableBytes (lookups: byte array list) =
    let list = Array.concat [ u16 lookups.Length; Array.concat (offsetsOf (2 + 2 * lookups.Length) lookups |> List.map u16); Array.concat lookups ]
    Array.concat [ u32 0x00010000; u16 10; u16 12; u16 14; u16 0; u16 0; list ]

/// <summary>Loads the GPOS table of a font made of these bytes.</summary>
/// <param name="gpos">The table bytes.</param>
/// <returns>The table, empty when the sanitizer rejected it.</returns>
let private loadGpos (gpos: byte array) =
    let file = SfntBuilder.sfnt 0x00010000u [ Tags.Gpos, gpos ]
    (OtFace.create (Face.create (ReadOnlyMemory file) 0u)).Gpos.Value

/// <summary>The font and direction the runs are positioned with.</summary>
let private font = Font.create Face.empty |> Font.withScale 1000 1000

/// <summary>The map that applies the lookups of a table at the given indices under mask 1, in order, in one stage.</summary>
/// <param name="indices">The lookup indices.</param>
/// <returns>The lookups and the stages.</returns>
let private mapFor (indices: int list) =
    let maps =
        indices
        |> List.map (fun index ->
            { LookupIndex = index
              AutoZwnj = false
              AutoZwj = false
              Random = false
              PerSyllable = false
              Mask = 1u
              FeatureTag = Tag.make 'm' 'a' 'r' 'k' })
    struct (ImmutableArray.CreateRange maps, ImmutableArray.Create { LastLookup = maps.Length; Pause = PauseKind.NoPause })

/// <summary>A run of glyphs under mask 1, one cluster each, at advances of 500.</summary>
/// <param name="glyphs">The glyph ids.</param>
/// <param name="classes">The glyph class of each glyph.</param>
/// <param name="direction">The direction.</param>
/// <returns>The run.</returns>
let private runOf (glyphs: uint32 array) (classes: uint16 array) (direction: Direction) =
    { GlyphRun.empty with
        Infos = ImmutableArray.CreateRange (glyphs |> Array.mapi (fun index glyph -> { Codepoint = glyph; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u }))
        Props = ImmutableArray.CreateRange (classes |> Array.map (fun glyphClass -> { Unchecked.defaultof<GlyphProps> with GlyphClass = glyphClass }))
        Positions = ImmutableArray.CreateRange (glyphs |> Array.map (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }))
        HavePositions = true
        ContentType = GlyphContent
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale } }

/// <summary>The literal transliteration of the upstream context apply the kernel is held to: one mutable buffer, every function a
/// line-by-line copy of its C++ original (src/hb-ot-layout-gsubgpos.hh: <c>match_input</c>, <c>match_lookahead</c>,
/// <c>match_backtrack</c>, <c>apply_lookup</c>, <c>context_apply_lookup</c>, <c>chain_context_apply_lookup</c>, <c>RuleSet::apply</c>,
/// <c>ChainRuleSet::apply</c>, the formats' <c>apply</c>, <c>recurse</c> and <c>PosLookup::dispatch_recurse_func</c>), the lookups
/// applied in order with no gate and no digest of the run; the skipping iterators and the decisions of the single, pair and mark
/// subtables are the primitives the kernel shares with it.</summary>
module private Model =
    /// <summary>The state of <c>hb_buffer_t</c> and <c>hb_ot_apply_context_t</c> the context lookups read and write.</summary>
    [<NoComparison; NoEquality>]
    type State =
        { Info: GlyphInfo array
          Props: GlyphProps array
          Pos: GlyphPosition array
          Len: int
          Flags: BufferFlags
          Level: ClusterLevel
          Direction: Direction
          Table: LayoutTable<PosSubtable>
          Gdef: Gdef
          mutable Idx: int
          mutable MaxOps: int
          mutable Successful: bool
          mutable Scratch: ScratchFlags
          mutable LookupMask: uint32
          mutable LookupProps: uint32
          mutable AutoZwnj: bool
          mutable AutoZwj: bool
          mutable LastBase: int
          mutable LastBaseUntil: int
          mutable NestingLeft: int }

    /// <summary>The glyph data a skipping iterator matches against.</summary>
    type Data =
        | Glyphs of ImmutableArray<uint32>
        | Classes of ClassDef * ImmutableArray<uint32>
        | Coverages of ImmutableArray<Coverage>
        | Always

    /// <summary>How a rule set matches a glyph, <c>match_func</c> and its data.</summary>
    type Kind =
        | KGlyph
        | KClass of ClassDef

    /// <summary>The maximum length of a context, <c>HB_MAX_CONTEXT_LENGTH</c>.</summary>
    [<Literal>]
    let MaxContextLength = 64

    /// <summary>The nesting budget, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let MaxNestingLevel = 64

    /// <summary>The ligature id, <c>_hb_glyph_info_get_lig_id</c>.</summary>
    let ligId (p: GlyphProps) = int (p.LigProps >>> 5)

    /// <summary>The ligature component, <c>_hb_glyph_info_get_lig_comp</c>.</summary>
    let ligComp (p: GlyphProps) = if p.LigProps &&& 0x10uy <> 0uy then 0 else int (p.LigProps &&& 0x0Fuy)

    /// <summary>Whether the buffer produces unsafe-to-concat flags.</summary>
    let produces (state: State) = state.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default

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

    /// <summary><c>_set_glyph_flags</c> on a buffer without output, where the output buffer is the buffer itself.</summary>
    let setGlyphFlags (state: State) (mask: uint32) (start: int) (finish: int) (interior: bool) (fromOutBuffer: bool) =
        if not (uint32 finish - uint32 start > 255u) then
            let finish = min finish state.Len
            if not (interior && not fromOutBuffer && finish - start < 2) then
                if not interior then
                    for i in start .. finish - 1 do
                        state.Info[i] <- { state.Info[i] with Mask = state.Info[i].Mask ||| mask }
                else
                    let cluster = findMinCluster state start finish UInt32.MaxValue
                    setInfosFlags state start finish cluster mask

    /// <summary><c>unsafe_to_break</c>.</summary>
    let unsafeToBreak (state: State) (start: int) (finish: int) = setGlyphFlags state 3u start finish true false

    /// <summary><c>unsafe_to_break_from_outbuffer</c>.</summary>
    let unsafeToBreakFromOutbuffer (state: State) (start: int) (finish: int) = setGlyphFlags state 3u start finish true true

    /// <summary><c>unsafe_to_concat</c>.</summary>
    let unsafeToConcat (state: State) (start: int) (finish: int) =
        if produces state then setGlyphFlags state 2u start finish false false

    /// <summary><c>unsafe_to_concat_from_outbuffer</c>.</summary>
    let unsafeToConcatFromOutbuffer (state: State) (start: int) (finish: int) =
        if produces state then setGlyphFlags state 2u start finish false true

    /// <summary>The matcher of an iterator, <c>skipping_iterator_t::init</c>.</summary>
    let matcher (state: State) (context: bool) =
        Matching.matcherFor 1 context state.LookupProps state.LookupMask state.AutoZwnj state.AutoZwj false 0uy

    /// <summary>The glyphs the iterators look at.</summary>
    let infosOf (state: State) = ReadOnlySpan<GlyphInfo>(state.Info, 0, state.Len)

    /// <summary>The properties the iterators look at.</summary>
    let propsOf (state: State) = ReadOnlySpan<GlyphProps>(state.Props, 0, state.Len)

    /// <summary><c>skipping_iterator_t::next</c> from an index with glyph data.</summary>
    let next (state: State) (context: bool) (data: Data) (position: int) (from: int) =
        match data with
        | Glyphs ids -> Matching.next (matcher state context) state.Gdef (GlyphIds ids) position (infosOf state) (propsOf state) from state.Len |> TailCalls.settle
        | Classes (classDef, values) -> Matching.next (matcher state context) state.Gdef (ClassValues (classDef, values)) position (infosOf state) (propsOf state) from state.Len |> TailCalls.settle
        | Coverages coverages -> Matching.next (matcher state context) state.Gdef (CoverageList coverages) position (infosOf state) (propsOf state) from state.Len |> TailCalls.settle
        | Always -> Matching.next (matcher state context) state.Gdef (MatchAlways ()) position (infosOf state) (propsOf state) from state.Len |> TailCalls.settle

    /// <summary><c>skipping_iterator_t::prev</c> from an index with glyph data.</summary>
    let prev (state: State) (data: Data) (position: int) (from: int) =
        match data with
        | Glyphs ids -> Matching.prev (matcher state true) state.Gdef (GlyphIds ids) position (infosOf state) (propsOf state) from |> TailCalls.settle
        | Classes (classDef, values) -> Matching.prev (matcher state true) state.Gdef (ClassValues (classDef, values)) position (infosOf state) (propsOf state) from |> TailCalls.settle
        | Coverages coverages -> Matching.prev (matcher state true) state.Gdef (CoverageList coverages) position (infosOf state) (propsOf state) from |> TailCalls.settle
        | Always -> Matching.prev (matcher state true) state.Gdef (MatchAlways ()) position (infosOf state) (propsOf state) from |> TailCalls.settle

    /// <summary><c>skipping_iterator_t::may_skip</c>.</summary>
    let maySkip (state: State) (context: bool) (index: int) =
        Matching.maySkip (matcher state context) state.Gdef state.Info[index] state.Props[index]

    /// <summary><c>match_input</c>, <c>match_func</c> and its data being the input values.</summary>
    let matchInput (state: State) (count: int) (input: Data) (positions: int array) =
        if count = 1 then
            positions[0] <- state.Idx
            struct (true, state.Idx + 1)
        elif count > MaxContextLength then struct (false, 0)
        else
            let firstLigId = ligId state.Props[state.Idx]
            let firstLigComp = ligComp state.Props[state.Idx]
            let mutable ligbase = 0
            let mutable idx = state.Idx
            let mutable i = 1
            let mutable verdict = ValueNone
            while verdict.IsNone && i < count do
                match next state false input (i - 1) idx with
                | Miss unsafeTo -> verdict <- ValueSome (struct (false, unsafeTo))
                | Hit hit ->
                    idx <- hit
                    positions[i] <- hit
                    let thisLigId = ligId state.Props[hit]
                    let thisLigComp = ligComp state.Props[hit]
                    if firstLigId <> 0 && firstLigComp <> 0 then
                        if firstLigId <> thisLigId || firstLigComp <> thisLigComp then
                            if ligbase = 0 then
                                let mutable found = false
                                let mutable j = state.Idx
                                while j > 0 && not found && ligId state.Props[j - 1] = firstLigId do
                                    if ligComp state.Props[j - 1] = 0 then
                                        j <- j - 1
                                        found <- true
                                    else j <- j - 1
                                ligbase <- (match found && maySkip state false j = SkipVerdict.SkipYes with | true -> 2 | false -> 1)
                            if ligbase = 1 then verdict <- ValueSome (struct (false, 0))
                    elif thisLigId <> 0 && thisLigComp <> 0 && thisLigId <> firstLigId then verdict <- ValueSome (struct (false, 0))
                    i <- i + 1
            match verdict with
            | ValueSome failed -> failed
            | ValueNone ->
                positions[0] <- state.Idx
                struct (true, idx + 1)

    /// <summary><c>match_lookahead</c>.</summary>
    let matchLookahead (state: State) (count: int) (lookahead: Data) (startIndex: int) =
        if count = 0 then struct (true, startIndex)
        else
            let mutable idx = startIndex - 1
            let mutable i = 0
            let mutable verdict = ValueNone
            while verdict.IsNone && i < count do
                match next state true lookahead i idx with
                | Miss unsafeTo -> verdict <- ValueSome (struct (false, unsafeTo))
                | Hit hit ->
                    idx <- hit
                    i <- i + 1
            match verdict with
            | ValueSome failed -> failed
            | ValueNone -> struct (true, idx + 1)

    /// <summary><c>match_backtrack</c>, whose buffer has no output: the backtrack is the run before the cursor.</summary>
    let matchBacktrack (state: State) (count: int) (backtrack: Data) =
        if count = 0 then struct (true, state.Idx)
        else
            let mutable idx = state.Idx
            let mutable i = 0
            let mutable verdict = ValueNone
            while verdict.IsNone && i < count do
                match prev state backtrack i idx with
                | Miss unsafeFrom -> verdict <- ValueSome (struct (false, unsafeFrom))
                | Hit hit ->
                    idx <- hit
                    i <- i + 1
            match verdict with
            | ValueSome failed -> failed
            | ValueNone -> struct (true, idx)

    /// <summary>The data a kind matches a sequence of values with.</summary>
    let dataOf (kind: Kind) (values: ImmutableArray<uint32>) =
        match kind with
        | KGlyph -> Glyphs values
        | KClass classDef -> Classes (classDef, values)

    /// <summary>The value a kind reads from a glyph, <c>get_value</c>.</summary>
    let valueOf (kind: Kind) (info: GlyphInfo) =
        match kind with
        | KGlyph -> info.Codepoint
        | KClass classDef -> ClassDef.classOf classDef info.Codepoint

    /// <summary>The digest of the first input values of a rule set, <c>collect_first_input_classes</c>.</summary>
    let ruleDigest (rules: ImmutableArray<Rule>) =
        let mutable mask = 0UL
        let mutable full = false
        for rule in rules do
            if not full then
                if rule.InputCount <= 1 then
                    full <- true
                    mask <- UInt64.MaxValue
                else mask <- mask ||| (1UL <<< int (rule.Input[0] &&& 63u))
        mask

    /// <summary>The digest of the first input values of a chained rule set.</summary>
    let chainDigest (rules: ImmutableArray<ChainRule>) =
        let mutable mask = 0UL
        let mutable full = false
        for rule in rules do
            if not full then
                if rule.ChainInputCount <= 1 then
                    full <- true
                    mask <- UInt64.MaxValue
                else mask <- mask ||| (1UL <<< int (rule.ChainInput[0] &&& 63u))
        mask

    /// <summary>The coverage index of a glyph, minus one for <c>NOT_COVERED</c>.</summary>
    let covered (coverage: Coverage) (glyph: uint32) =
        match Coverage.index coverage glyph with
        | Covered index -> index
        | NotCovered -> -1

    /// <summary>A rule set whose index is past the array, the Null object, which has no rules.</summary>
    let setAt (sets: ImmutableArray<ImmutableArray<'R>>) (index: int) =
        if index < sets.Length then sets[index] else ImmutableArray<'R>.Empty

    /// <summary>The environment of the decisions of a nested lookup's single, pair and mark subtables.</summary>
    let envOf (state: State) =
        { Font = font
          Direction = state.Direction
          Horizontal = Direction.isHorizontal state.Direction
          Matcher = matcher state false
          Gdef = state.Gdef }

    /// <summary>The saturating sum, <c>hb_saturate_add</c>.</summary>
    let satAdd (a: int) (b: int) =
        let r = int64 a + int64 b
        if r > int64 Int32.MaxValue then Int32.MaxValue elif r < int64 Int32.MinValue then Int32.MinValue else int r

    /// <summary>Writes what a single or pair subtable decided at an index.</summary>
    let commit (state: State) (index: int) (outcome: PosOutcome) =
        if outcome.Advance <> 0 then state.Pos[index] <- { state.Pos[index] with XAdvance = satAdd state.Pos[index].XAdvance outcome.Advance }
        if outcome.FirstWorked then state.Pos[index] <- outcome.First
        if outcome.SecondWorked then state.Pos[outcome.SecondIndex] <- outcome.Second
        if outcome.ConcatTo <> 0 && produces state then setGlyphFlags state 2u index outcome.ConcatTo false false
        if outcome.Break1To <> 0 then setGlyphFlags state 3u index outcome.Break1To true false
        if outcome.Break2To <> 0 then setGlyphFlags state 3u index outcome.Break2To true false

    /// <summary>Writes what an attachment subtable decided at an index: a mark's position and flags.</summary>
    let commitAttach (state: State) (index: int) (outcome: AttachOutcome) =
        state.LastBase <- outcome.LastBase
        state.LastBaseUntil <- outcome.LastBaseUntil
        if outcome.ConcatFrom >= 0 && produces state then setGlyphFlags state 2u outcome.ConcatFrom (index + 1) false false
        if outcome.Kind = AttachKind.Mark then
            setGlyphFlags state 3u outcome.BreakFrom outcome.BreakTo true false
            state.Pos[index] <- outcome.Position
            if outcome.Attached then state.Scratch <- state.Scratch ||| ScratchFlags.HasGposAttachment

    /// <summary><c>apply_lookup</c> of a buffer without output: the length never changes, so the match positions are not adjusted
    /// and the cursor ends on the end of the match.</summary>
    let rec applyLookup (state: State) (count: int) (records: ImmutableArray<LookupRecord>) (positions: int array) (matchEnd: int) =
        let mutable i = 0
        let mutable stop = false
        while i < records.Length && state.Successful && not stop do
            let idx = records[i].SequenceIndex
            if idx < count then
                let origLen = state.Len
                if positions[idx] < origLen then
                    state.Idx <- positions[idx]
                    if state.MaxOps <= 0 then stop <- true
                    else recurse state records[i].LookupListIndex |> ignore
            i <- i + 1
        state.Idx <- matchEnd

    /// <summary><c>recurse</c> and <c>PosLookup::dispatch_recurse_func</c>.</summary>
    and recurse (state: State) (lookupIndex: int) =
        if state.NestingLeft = 0 then
            state.Successful <- false
            false
        else
            state.MaxOps <- state.MaxOps - 1
            if state.MaxOps < 0 then
                state.Successful <- false
                false
            else
                state.NestingLeft <- state.NestingLeft - 1
                let result =
                    if lookupIndex >= state.Table.Lookups.Length then false
                    else
                        let accel = state.Table.Lookups[lookupIndex].Value
                        let saved = state.LookupProps
                        state.LookupProps <- Gdef.sanitizeLookupProps state.Gdef accel.Props
                        let applied = applyAccel state accel
                        state.LookupProps <- saved
                        applied
                state.NestingLeft <- state.NestingLeft + 1
                result

    /// <summary>The subtables of a lookup tried in order on the current glyph, <c>hb_ot_layout_lookup_accelerator_t::apply</c>.</summary>
    and applyAccel (state: State) (accel: LookupAccel<PosSubtable>) =
        let count = accel.Subtables.Length
        let mutable applied = false
        let mutable i = 0
        while not applied && i < count do
            let sub = accel.Subtables[i]
            if count = 1 || Digest.mayHaveCall state.Info[state.Idx].Codepoint sub.SubtableDigest then
                applied <- applySubtable state sub.Subtable i
            i <- i + 1
        applied

    /// <summary>One subtable's apply on the current glyph, whatever its type.</summary>
    and applySubtable (state: State) (subtable: PosSubtable) (position: int) =
        match subtable with
        | PosContext rules -> contextApply state rules position
        | PosChainContext rules -> chainApply state rules position
        | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ ->
            let outcome = GposLookups.applySubtable (envOf state) subtable (infosOf state) (propsOf state) (ReadOnlySpan<GlyphPosition>(state.Pos, 0, state.Len)) state.Idx |> TailCalls.settle
            commit state state.Idx outcome
            if outcome.Applied then state.Idx <- outcome.Next
            outcome.Applied
        | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | CursivePos _ ->
            let outcome = GposAttach.applySubtable (envOf state) subtable (infosOf state) (propsOf state) (ReadOnlySpan<GlyphPosition>(state.Pos, 0, state.Len)) state.Idx state.LastBase state.LastBaseUntil |> TailCalls.settle
            commitAttach state state.Idx outcome
            if outcome.Applied then state.Idx <- outcome.Next
            outcome.Applied
        | PosPending _ -> false

    /// <summary><c>context_apply_lookup</c>.</summary>
    and contextApplyLookup (state: State) (inputCount: int) (input: Data) (records: ImmutableArray<LookupRecord>) (positions: int array) =
        if inputCount > MaxContextLength then false
        else
            let struct (matched, matchEnd) = matchInput state inputCount input positions
            if matched then
                unsafeToBreak state state.Idx matchEnd
                applyLookup state inputCount records positions matchEnd
                true
            else
                unsafeToConcat state state.Idx matchEnd
                false

    /// <summary><c>Rule::apply</c>.</summary>
    and ruleApply (state: State) (kind: Kind) (rule: Rule) (positions: int array) =
        contextApplyLookup state rule.InputCount (dataOf kind rule.Input) rule.Records positions

    /// <summary><c>RuleSet::apply</c>, with the fast path for more than four rules.</summary>
    and ruleSetApply (state: State) (kind: Kind) (rules: ImmutableArray<Rule>) (digest: uint64 voption) (positions: int array) =
        let slow () = rules |> Seq.exists (fun rule -> ruleApply state kind rule positions)
        if rules.Length <= 4 then slow ()
        else
            match next state true Always 0 state.Idx with
            | Miss unsafeTo1 ->
                let mutable unsafeToConcatFlag = false
                let applied =
                    rules
                    |> Seq.filter (fun rule -> if rule.InputCount <= 1 then true else (unsafeToConcatFlag <- true; false))
                    |> Seq.exists (fun rule -> ruleApply state kind rule positions)
                if unsafeToConcatFlag then unsafeToConcat state state.Idx unsafeTo1
                applied
            | Hit firstIndex when maySkip state true firstIndex <> SkipVerdict.SkipNo -> slow ()
            | Hit firstIndex ->
                let first = state.Info[firstIndex]
                let unsafeTo1 = firstIndex + 1
                let digestRejects =
                    match kind, digest with
                    | KClass _, ValueSome mask -> (mask &&& (1UL <<< int (valueOf kind first &&& 63u))) = 0UL
                    | KClass _, ValueNone -> false
                    | KGlyph, _ -> false
                if digestRejects then
                    unsafeToConcat state state.Idx unsafeTo1
                    false
                else
                    match next state true Always 0 firstIndex with
                    | Hit secondIndex when maySkip state true secondIndex <> SkipVerdict.SkipNo -> slow ()
                    | probe ->
                        let second, unsafeTo2 = (match probe with | Hit secondIndex -> ValueSome state.Info[secondIndex], secondIndex + 1 | Miss _ -> ValueNone, 0)
                        let matches (info: GlyphInfo) (value: uint32) = valueOf kind info = value
                        let mutable unsafeTo = -1
                        let mutable i = 0
                        let mutable result = false
                        let mutable finished = false
                        while not finished && i < rules.Length do
                            let rule = rules[i]
                            if rule.InputCount <= 1 || matches first rule.Input[0] then
                                if second.IsNone || rule.InputCount <= 2 || matches second.Value rule.Input[1] then
                                    if ruleApply state kind rule positions then
                                        if unsafeTo <> -1 then unsafeToConcat state state.Idx unsafeTo
                                        result <- true
                                        finished <- true
                                else unsafeTo <- unsafeTo2
                            else
                                if unsafeTo = -1 then unsafeTo <- unsafeTo1
                                let mutable go = true
                                while go && i + 1 < rules.Length do
                                    let next = rules[i + 1]
                                    if next.InputCount <= 1 || next.Input[0] <> rule.Input[0] then go <- false else i <- i + 1
                            i <- i + 1
                        if not finished && unsafeTo <> -1 then unsafeToConcat state state.Idx unsafeTo
                        result

    /// <summary><c>Context::apply</c>, formats 1 to 6.</summary>
    and contextApply (state: State) (rules: ContextRules) (position: int) =
        let positions = Array.zeroCreate<int> MaxContextLength
        let glyph = state.Info[state.Idx].Codepoint
        match rules with
        | GlyphRules (coverage, ruleSets) ->
            let index = covered coverage glyph
            if index < 0 then false else ruleSetApply state KGlyph (setAt ruleSets index) ValueNone positions
        | ClassRules (coverage, classDef, ruleSets, _) ->
            if covered coverage glyph < 0 then false
            else
                let klass = int (ClassDef.classOf classDef glyph)
                let set = setAt ruleSets klass
                let digest = if position < 8 && klass < ruleSets.Length then ValueSome (ruleDigest set) else ValueNone
                ruleSetApply state (KClass classDef) set digest positions
        | CoverageRule (coverages, records) ->
            if coverages.IsEmpty || covered coverages[0] glyph < 0 then false
            else contextApplyLookup state coverages.Length (Coverages (coverages.RemoveAt 0)) records positions

    /// <summary><c>chain_context_apply_lookup</c>.</summary>
    and chainContextApplyLookup (state: State) (backtrackCount: int) (backtrack: Data) (inputCount: int) (input: Data) (lookaheadCount: int) (lookahead: Data) (records: ImmutableArray<LookupRecord>) (positions: int array) =
        if inputCount > MaxContextLength then false
        else
            let struct (matched, matchEnd) = matchInput state inputCount input positions
            if not matched then
                unsafeToConcat state state.Idx state.Idx
                false
            else
                let struct (lookaheadMatched, endIndex) = matchLookahead state lookaheadCount lookahead matchEnd
                if not lookaheadMatched then
                    unsafeToConcat state state.Idx endIndex
                    false
                else
                    let struct (backtrackMatched, startIndex) = matchBacktrack state backtrackCount backtrack
                    if not backtrackMatched then
                        unsafeToConcatFromOutbuffer state startIndex endIndex
                        false
                    else
                        unsafeToBreakFromOutbuffer state startIndex endIndex
                        applyLookup state inputCount records positions matchEnd
                        true

    /// <summary><c>ChainRule::apply</c>.</summary>
    and chainRuleApply (state: State) (kinds: Kind * Kind * Kind) (rule: ChainRule) (positions: int array) =
        let backtrackKind, inputKind, lookaheadKind = kinds
        chainContextApplyLookup state rule.Backtrack.Length (dataOf backtrackKind rule.Backtrack) rule.ChainInputCount (dataOf inputKind rule.ChainInput) rule.Lookahead.Length (dataOf lookaheadKind rule.Lookahead) rule.ChainRecords positions

    /// <summary><c>ChainRuleSet::apply</c>, with the fast path for more than four rules.</summary>
    and chainRuleSetApply (state: State) (kinds: Kind * Kind * Kind) (rules: ImmutableArray<ChainRule>) (digest: uint64 voption) (positions: int array) =
        let _, inputKind, lookaheadKind = kinds
        let slow () = rules |> Seq.exists (fun rule -> chainRuleApply state kinds rule positions)
        if rules.Length <= 4 then slow ()
        else
            match next state true Always 0 state.Idx with
            | Miss unsafeTo1 ->
                let mutable unsafeToConcatFlag = false
                let applied =
                    rules
                    |> Seq.filter (fun rule -> if rule.ChainInputCount <= 1 && rule.Lookahead.Length = 0 then true else (unsafeToConcatFlag <- true; false))
                    |> Seq.exists (fun rule -> chainRuleApply state kinds rule positions)
                if unsafeToConcatFlag then unsafeToConcat state state.Idx unsafeTo1
                applied
            | Hit firstIndex when maySkip state true firstIndex <> SkipVerdict.SkipNo -> slow ()
            | Hit firstIndex ->
                let first = state.Info[firstIndex]
                let unsafeTo1 = firstIndex + 1
                let digestRejects =
                    match inputKind, digest with
                    | KClass _, ValueSome mask -> (mask &&& (1UL <<< int (valueOf inputKind first &&& 63u))) = 0UL
                    | KClass _, ValueNone -> false
                    | KGlyph, _ -> false
                if digestRejects then
                    unsafeToConcat state state.Idx unsafeTo1
                    false
                else
                    match next state true Always 0 firstIndex with
                    | Hit secondIndex when maySkip state true secondIndex <> SkipVerdict.SkipNo -> slow ()
                    | probe ->
                        let second, unsafeTo2 = (match probe with | Hit secondIndex -> ValueSome state.Info[secondIndex], secondIndex + 1 | Miss _ -> ValueNone, 0)
                        let mutable unsafeTo = -1
                        let mutable i = 0
                        let mutable result = false
                        let mutable finished = false
                        while not finished && i < rules.Length do
                            let rule = rules[i]
                            let mutable lenP1 = rule.ChainInputCount
                            let firstMatches =
                                if lenP1 > 1 then valueOf inputKind first = rule.ChainInput[0]
                                else rule.Lookahead.Length = 0 || valueOf lookaheadKind first = rule.Lookahead[0]
                            if firstMatches then
                                lenP1 <- max lenP1 1
                                let secondMatches =
                                    second.IsNone
                                    || (if lenP1 > 2 then valueOf inputKind second.Value = rule.ChainInput[1]
                                        else rule.Lookahead.Length <= 2 - lenP1 || valueOf lookaheadKind second.Value = rule.Lookahead[2 - lenP1])
                                if secondMatches then
                                    if chainRuleApply state kinds rule positions then
                                        if unsafeTo <> -1 then unsafeToConcat state state.Idx unsafeTo
                                        result <- true
                                        finished <- true
                                else unsafeTo <- unsafeTo2
                            else
                                if unsafeTo = -1 then unsafeTo <- unsafeTo1
                                if lenP1 > 1 then
                                    let mutable go = true
                                    while go && i + 1 < rules.Length do
                                        let next = rules[i + 1]
                                        if next.ChainInputCount <= 1 || next.ChainInput[0] <> rule.ChainInput[0] then go <- false else i <- i + 1
                            i <- i + 1
                        if not finished && unsafeTo <> -1 then unsafeToConcat state state.Idx unsafeTo
                        result

    /// <summary><c>ChainContext::apply</c>, formats 1 to 5.</summary>
    and chainApply (state: State) (rules: ChainRules) (position: int) =
        let positions = Array.zeroCreate<int> MaxContextLength
        let glyph = state.Info[state.Idx].Codepoint
        match rules with
        | ChainGlyphRules (coverage, ruleSets) ->
            let index = covered coverage glyph
            if index < 0 then false else chainRuleSetApply state (KGlyph, KGlyph, KGlyph) (setAt ruleSets index) ValueNone positions
        | ChainClassRules (coverage, backtrackClasses, inputClasses, lookaheadClasses, ruleSets, _) ->
            if covered coverage glyph < 0 then false
            else
                let klass = int (ClassDef.classOf inputClasses glyph)
                let set = setAt ruleSets klass
                let digest = if position < 8 && klass < ruleSets.Length then ValueSome (chainDigest set) else ValueNone
                chainRuleSetApply state (KClass backtrackClasses, KClass inputClasses, KClass lookaheadClasses) set digest positions
        | ChainCoverageRule (backtrack, input, lookahead, records) ->
            if input.IsEmpty || covered input[0] glyph < 0 then false
            else chainContextApplyLookup state backtrack.Length (Coverages backtrack) input.Length (Coverages (input.RemoveAt 0)) lookahead.Length (Coverages lookahead) records positions

    /// <summary><c>apply_forward</c> over one lookup.</summary>
    let applyForward (state: State) (accel: LookupAccel<PosSubtable>) =
        state.Idx <- 0
        let mutable running = true
        while running && state.Successful do
            let mutable j = state.Idx
            while j < state.Len && not (Digest.mayHaveCall state.Info[j].Codepoint accel.LookupDigest && (state.Info[j].Mask &&& state.LookupMask) <> 0u && Matching.checkGlyphProperty state.Gdef state.Info[j].Codepoint state.Props[j].GlyphClass state.LookupProps) do
                j <- j + 1
            state.Idx <- j
            if state.Idx >= state.Len then running <- false
            elif not (applyAccel state accel) then state.Idx <- state.Idx + 1

    /// <summary>Positions a run as <c>hb_ot_map_t::apply</c> does for GPOS, every lookup of the map in order, none gated.</summary>
    let position (table: LayoutTable<PosSubtable>) (maps: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (run: GlyphRun) =
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
              Table = table
              Gdef = font.Ot.Gdef.Value
              Idx = 0
              MaxOps = run.MaxOps
              Successful = run.Successful
              Scratch = run.ScratchFlags
              LookupMask = 1u
              LookupProps = 0u
              AutoZwnj = true
              AutoZwj = true
              LastBase = -1
              LastBaseUntil = 0
              NestingLeft = MaxNestingLevel }
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
                    applyForward state accel
                i <- i + 1
        state

/// <summary>Whether two runs agree on every field the positioning pass writes.</summary>
/// <param name="what">What the runs are.</param>
/// <param name="model">The state the literal transliteration ended in.</param>
/// <param name="actual">The run the kernel produced.</param>
let private expectAgree (what: string) (model: Model.State) (actual: GlyphRun) =
    Expect.sequenceEqual (actual.Infos.AsSpan().ToArray()) model.Info (what + ": infos, with the glyph flags")
    Expect.sequenceEqual (actual.Positions.AsSpan().ToArray()) model.Pos (what + ": positions")
    Expect.equal actual.MaxOps model.MaxOps (what + ": operation budget")
    Expect.equal actual.Successful model.Successful (what + ": success")
    Expect.equal actual.ScratchFlags model.Scratch (what + ": scratch flags")

/// <summary>The glyphs of the random tables: three bases, two marks and a ligature.</summary>
let private universe = [ 1; 2; 3; 5; 6; 7 ]

/// <summary>The glyph class of a glyph of the universe: bases, marks and a ligature.</summary>
/// <param name="glyph">The glyph.</param>
/// <returns>The class.</returns>
let private classOfGlyph (glyph: int) =
    match glyph with
    | 5 | 6 -> GlyphPropsBits.Mark
    | 7 -> GlyphPropsBits.Ligature
    | _ -> GlyphPropsBits.Base

/// <summary>Builds a random table of lookups: single, pair and mark-to-base adjustments, and context and chained-context lookups
/// of every format, some in extensions and some with two subtables, whose records name other lookups, themselves, and lookups
/// that do not exist.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The table bytes.</returns>
let private randomTable (rnd: Random) =
    let total = 3 + rnd.Next 5
    let pick (items: 'a list) = items[rnd.Next items.Length]
    let subset () =
        let chosen = universe |> List.filter (fun _ -> rnd.Next 2 = 0)
        match chosen with
        | [] -> [ pick universe ]
        | _ -> chosen
    let sequence (maximum: int) = List.init (rnd.Next (maximum + 1)) (fun _ -> subset ())
    let glyphs (maximum: int) = List.init (rnd.Next (maximum + 1)) (fun _ -> pick universe)
    let records () = List.init (rnd.Next 4) (fun _ -> rnd.Next 5, (match rnd.Next 12 with | 0 -> total + 1 | _ -> rnd.Next total))
    let classes () = universe |> List.choose (fun glyph -> match rnd.Next 3 with | 0 -> None | klass -> Some (glyph, klass))
    let flag () = pick [ 0; 0; 0; 0; 0x0008; 0x0002; 0x0004; 0x000A ]
    let wrap (innerType: int) (subtable: byte array) =
        match rnd.Next 4 with
        | 0 -> 9, extension innerType subtable
        | _ -> innerType, subtable
    let contextSubtable () =
        match rnd.Next 3 with
        | 0 ->
            let covered = subset ()
            context1 covered (covered |> List.map (fun _ -> ruleSetBytes (List.init (rnd.Next 7) (fun _ -> ruleBytes (glyphs 3) (records ())))))
        | 1 ->
            let covered = subset ()
            context2 covered (classes ()) (List.init (1 + rnd.Next 3) (fun _ -> ruleSetBytes (List.init (rnd.Next 7) (fun _ -> ruleBytes (List.init (rnd.Next 3) (fun _ -> rnd.Next 3)) (records ())))))
        | _ -> context3 (subset () :: sequence 3) (records ())
    let chainSubtable () =
        match rnd.Next 3 with
        | 0 ->
            let covered = subset ()
            chain1 covered (covered |> List.map (fun _ -> ruleSetBytes (List.init (rnd.Next 7) (fun _ -> chainRuleBytes (glyphs 2) (glyphs 2) (glyphs 2) (records ())))))
        | 1 ->
            let covered = subset ()
            chain2 covered (classes ()) (classes ()) (classes ()) (List.init (1 + rnd.Next 3) (fun _ -> ruleSetBytes (List.init (rnd.Next 7) (fun _ -> chainRuleBytes (List.init (rnd.Next 3) (fun _ -> rnd.Next 3)) (List.init (rnd.Next 3) (fun _ -> rnd.Next 3)) (List.init (rnd.Next 3) (fun _ -> rnd.Next 3)) (records ())))))
        | _ -> chain3 (sequence 2) (subset () :: sequence 2) (sequence 2) (records ())
    let simple (index: int) =
        match index < 2, rnd.Next 4 with
        | true, _ | false, 0 -> lookupBytes 1 (flag ()) [ singleBytes (subset ()) (1 + rnd.Next 300) ]
        | false, 1 -> lookupBytes 2 (flag ()) [ pairBytes (pick universe) (pick universe) (1 + rnd.Next 300) ]
        | false, _ -> lookupBytes 4 (flag ()) [ markBaseBytes () ]
    let contextual () =
        let subtables = List.init (1 + rnd.Next 2) (fun _ -> match rnd.Next 2 with | 0 -> 7, contextSubtable () | _ -> 8, chainSubtable ())
        let lookupType = fst subtables.Head
        let sameKind = subtables |> List.filter (fun (kind, _) -> kind = lookupType)
        let wrapped = sameKind |> List.map (fun (kind, body) -> wrap kind body)
        match wrapped |> List.exists (fun (outer, _) -> outer = 9) with
        | true -> lookupBytes 9 (flag ()) (sameKind |> List.map (fun (kind, body) -> extension kind body))
        | false -> lookupBytes lookupType (flag ()) (wrapped |> List.map snd)
    let lookups = List.init total (fun index -> match index < 2 || rnd.Next 3 = 0 with | true -> simple index | false -> contextual ())
    tableBytes lookups


/// <summary>The number of random tables of the property that the contexts changed a position of, that left an unsafe-to-concat
/// mark, and that ran the operation budget out.</summary>
let private counters = [| 0; 0; 0 |]

/// <summary>Positions a run through the kernel with the lookups of a table at the given indices, with the sets of the map and
/// without them, and checks every field of the result against the literal transliteration.</summary>
/// <param name="table">The table.</param>
/// <param name="indices">The lookup indices the map applies.</param>
/// <param name="run">The run.</param>
/// <returns>The positioned run.</returns>
let private positionWith (table: LayoutTable<PosSubtable>) (indices: int list) (run: GlyphRun) =
    let struct (maps, stages) = mapFor indices
    let gdef = font.Ot.Gdef.Value
    let covered = Engine.gposLookupCoveredOf table maps
    let glyphLookups = Engine.gposGlyphLookupsOf table maps
    let planned = Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run
    let ungated = Engine.positionTable font table gdef maps stages ImmutableArray.Empty ImmutableArray.Empty ImmutableArray.Empty run
    expectSameRunAll "the kernel through the plan's sets equals the kernel without them" ungated planned
    expectAgree "the kernel equals the literal transliteration" (Model.position table maps stages run) planned
    planned

/// <summary>The advances of a positioned run.</summary>
/// <param name="run">The run.</param>
/// <returns>The horizontal advances.</returns>
let private advances (run: GlyphRun) =
    run.Positions.AsSpan().ToArray() |> Array.map (fun position -> position.XAdvance)

/// <summary>The masks of a positioned run.</summary>
/// <param name="run">The run.</param>
/// <returns>The masks.</returns>
let private masks (run: GlyphRun) =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask)

/// <summary>A run of base glyphs.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The run.</returns>
let private basesOf (glyphs: uint32 array) =
    runOf glyphs (glyphs |> Array.map (fun _ -> GlyphPropsBits.Base)) Direction.Ltr

/// <summary>The lookups 0 and 1 of the tables of the fixed cases: single adjustments of glyph 1 by 100 and of glyph 3 by 25.</summary>
let private singles = [ lookupBytes 1 0 [ singleBytes [ 1 ] 100 ]; lookupBytes 1 0 [ singleBytes [ 3 ] 25 ] ]

/// <summary>A chain of context lookups that name one another and end in an adjustment of glyph 1 by 100.</summary>
/// <param name="depth">The number of context lookups before the adjustment.</param>
/// <returns>The table.</returns>
let private chainOf (depth: int) =
    loadGpos (tableBytes ([ for index in 0 .. depth - 1 -> lookupBytes 7 0 [ context3 [ [ 1 ] ] [ 0, index + 1 ] ] ] @ [ lookupBytes 1 0 [ singleBytes [ 1 ] 100 ] ]))

/// <summary>The P5.S12 context, chained-context and extension cases of GPOS.</summary>
[<Tests>]
let tests =
    testList "P5.S12" [
        test "in-place recursion keeps the positions: records at the first and last glyph of a match change those glyphs and no other" {
            let table = loadGpos (tableBytes (singles @ [ lookupBytes 7 0 [ context3 [ [ 1 ]; [ 2 ]; [ 3 ] ] [ 0, 0; 2, 1 ] ] ]))
            Expect.equal table.Lookups.Length 3 "the table is accepted"
            let positioned = positionWith table [ 2 ] (basesOf [| 4u; 1u; 2u; 3u; 4u |])
            Expect.sequenceEqual (advances positioned) [| 500; 600; 500; 525; 500 |] "the adjustments of the first and last glyph of the match apply there"
            Expect.sequenceEqual (masks positioned) [| 1u; 1u; 3u; 3u; 1u |] "unsafe_to_break (1, 4) marks the glyphs after the first cluster of the match (src/hb-buffer.hh:507-512)"
            Expect.equal positioned.Infos.Length 5 "no glyph is added or removed"
            let alone = positionWith table [ 0 ] (basesOf [| 4u; 1u; 2u; 3u; 4u |])
            Expect.sequenceEqual (advances alone) [| 500; 600; 500; 500; 500 |] "the adjustment alone runs at its glyph, so the context is what moved the third glyph"
        }

        test "a nested lookup with IgnoreMarks applies on a mark glyph: the recursion makes no property check" {
            let table = loadGpos (tableBytes [ lookupBytes 1 0x0008 [ singleBytes [ 5 ] 40 ]; lookupBytes 7 0 [ context3 [ [ 1 ]; [ 5 ] ] [ 1, 0 ] ] ])
            let run = runOf [| 1u; 5u |] [| GlyphPropsBits.Base; GlyphPropsBits.Mark |] Direction.Ltr
            Expect.sequenceEqual (advances (positionWith table [ 1 ] run)) [| 500; 540 |] "the nested adjustment applies to the mark"
            Expect.sequenceEqual (advances (positionWith table [ 0 ] run)) [| 500; 500 |] "the same lookup at the top level skips the mark"
        }

        test "a context lookup with IgnoreMarks skips marks between its input glyphs, and its nested lookup lands on the glyph after them" {
            let table = loadGpos (tableBytes [ lookupBytes 1 0 [ singleBytes [ 2 ] 60 ]; lookupBytes 7 0x0008 [ context3 [ [ 1 ]; [ 2 ] ] [ 1, 0 ] ] ])
            let run = runOf [| 1u; 5u; 2u |] [| GlyphPropsBits.Base; GlyphPropsBits.Mark; GlyphPropsBits.Base |] Direction.Ltr
            Expect.sequenceEqual (advances (positionWith table [ 1 ] run)) [| 500; 500; 560 |] "the match skips the mark and the second input glyph is the third glyph"
        }

        test "the nesting limit: 64 recursions apply and the 65th reaches the limit and fails the buffer" {
            let applied = positionWith (chainOf 64) [ 0 ] (basesOf [| 1u |])
            Expect.sequenceEqual (advances applied) [| 600 |] "the first lookup runs at the top level and names a chain of 64 more: 64 recursions"
            Expect.isTrue applied.Successful "the buffer is whole"
            let refused = positionWith (chainOf 65) [ 0 ] (basesOf [| 1u |])
            Expect.sequenceEqual (advances refused) [| 500 |] "a 65th recursion is refused before the adjustment applies"
            Expect.isFalse refused.Successful "recurse sets the buffer's success to false (src/hb-ot-layout-gsubgpos.hh:1144-1148)"
            let recursive = loadGpos (tableBytes [ lookupBytes 7 0 [ context3 [ [ 1 ] ] [ 0, 0 ] ] ])
            Expect.isFalse (positionWith recursive [ 0 ] (basesOf [| 1u |])).Successful "a lookup that names itself stops at the limit"
        }

        test "the operation budget: each recursion takes one operation and the records stop when none is left" {
            let table = loadGpos (tableBytes (singles @ [ lookupBytes 7 0 [ context3 [ [ 1 ] ] (List.replicate 10 (0, 0)) ] ]))
            let limited = positionWith table [ 2 ] { basesOf [| 1u |] with MaxOps = 3 }
            Expect.sequenceEqual (advances limited) [| 800 |] "three records recurse"
            Expect.equal limited.MaxOps 0 "and the budget is spent"
            Expect.isTrue limited.Successful "without failing the buffer"
            let one = positionWith table [ 2 ] { basesOf [| 1u |] with MaxOps = 1 }
            Expect.sequenceEqual (advances one) [| 600 |] "one operation, one recursion"
            let none = positionWith table [ 2 ] { basesOf [| 1u |] with MaxOps = 0 }
            Expect.sequenceEqual (advances none) [| 500 |] "no operation, no recursion"
        }

        test "a record past the match, a position past the run and a lookup past the table recurse into nothing" {
            let table = loadGpos (tableBytes (singles @ [ lookupBytes 7 0 [ context3 [ [ 1 ]; [ 2 ] ] [ 2, 0; 1, 9; 0, 0 ] ] ]))
            Expect.sequenceEqual (advances (positionWith table [ 2 ] (basesOf [| 1u; 2u |]))) [| 600; 500 |] "only the third record applies, at the first glyph"
        }

        test "every format of context and chained context, plain and in an extension, applies" {
            let nested = singles
            let contexts =
                [ lookupBytes 7 0 [ context1 [ 1 ] [ ruleSetBytes [ ruleBytes [ 2 ] [ 0, 0 ] ] ] ]
                  lookupBytes 7 0 [ context2 [ 1 ] [ 1, 1; 2, 2 ] [ ruleSetBytes []; ruleSetBytes [ ruleBytes [ 2 ] [ 0, 0 ] ] ] ]
                  lookupBytes 7 0 [ context3 [ [ 1 ]; [ 2 ] ] [ 0, 0 ] ]
                  lookupBytes 8 0 [ chain1 [ 1 ] [ ruleSetBytes [ chainRuleBytes [ 3 ] [ 2 ] [ 3 ] [ 0, 0 ] ] ] ]
                  lookupBytes 8 0 [ chain2 [ 1 ] [ 3, 1 ] [ 1, 1; 2, 2 ] [ 3, 1 ] [ ruleSetBytes []; ruleSetBytes [ chainRuleBytes [ 1 ] [ 2 ] [ 1 ] [ 0, 0 ] ] ] ]
                  lookupBytes 8 0 [ chain3 [ [ 3 ] ] [ [ 1 ]; [ 2 ] ] [ [ 3 ] ] [ 0, 0 ] ]
                  lookupBytes 9 0 [ extension 7 (context3 [ [ 1 ]; [ 2 ] ] [ 0, 0 ]) ]
                  lookupBytes 9 0 [ extension 8 (chain3 [ [ 3 ] ] [ [ 1 ]; [ 2 ] ] [ [ 3 ] ] [ 0, 0 ]) ] ]
            for index, lookup in List.indexed contexts do
                let table = loadGpos (tableBytes (nested @ [ lookup ]))
                Expect.equal table.Lookups.Length 3 $"lookup {index}: the table is accepted"
                let run = basesOf [| 3u; 1u; 2u; 3u |]
                let positioned = positionWith table [ 2 ] run
                match index with
                | 0 | 1 | 2 | 6 -> Expect.sequenceEqual (advances positioned) [| 500; 600; 500; 500 |] $"lookup {index}: the adjustment applies to the first glyph of the match"
                | 3 | 4 | 5 | 7 -> Expect.sequenceEqual (advances positioned) [| 500; 600; 500; 500 |] $"lookup {index}: the adjustment applies to the first glyph of the match, between its backtrack and lookahead"
                | _ -> ()
                let without = positionWith table [ 2 ] (basesOf [| 3u; 2u; 1u; 3u |])
                Expect.sequenceEqual (advances without) [| 500; 500; 500; 500 |] $"lookup {index}: the glyphs in another order match nothing"
        }

        test "the subtables of the types are parsed and digested: a context lookup's coverage is its start glyph set and nothing else" {
            let table = loadGpos (tableBytes (singles @ [ lookupBytes 7 0 [ context3 [ [ 2 ]; [ 3 ] ] [ 0, 1 ] ] ]))
            let struct (maps, _) = mapFor [ 2 ]
            let covered = Engine.gposLookupCoveredOf table maps
            let inSet (set: ImmutableArray<uint64>) (glyph: int) = glyph >>> 6 < set.Length && (set[glyph >>> 6] >>> (glyph &&& 63)) &&& 1UL <> 0UL
            Expect.isTrue (inSet covered[2] 2) "the first coverage's glyph starts a match"
            Expect.isFalse (inSet covered[2] 3) "the second input glyph does not"
            Expect.isFalse (inSet covered[2] 1) "nor does a glyph only a nested lookup covers"
            Expect.isTrue covered[0].IsEmpty "a lookup outside the map has no set"
            match table.Lookups[2].Value.Subtables[0].Subtable with
            | PosContext (CoverageRule _) -> ()
            | _ -> failtest "a context subtable of the third format"
        }

        test "sanitize: a context subtable whose offsets or counts lie outside the table rejects the table, as HarfBuzz does" {
            let valid = context1 [ 1 ] [ ruleSetBytes [ ruleBytes [ 2 ] [ 0, 0 ] ] ]
            let accepted (lookups: byte array list) = (loadGpos (tableBytes lookups)).Lookups.Length
            Expect.equal (accepted [ lookupBytes 7 0 [ valid ] ]) 1 "a valid subtable is accepted"
            let corrupt (at: int) (value: int) (bytes: byte array) = Array.mapi (fun index b -> match index - at with | 0 -> byte (value >>> 8) | 1 -> byte value | _ -> b) bytes
            Expect.equal (accepted [ lookupBytes 7 0 [ corrupt 2 0xFFF0 valid ] ]) 0 "a coverage offset past the table"
            Expect.equal (accepted [ lookupBytes 7 0 [ corrupt 4 0x7FFF valid ] ]) 0 "a rule set count that runs past the table"
            Expect.equal (accepted [ lookupBytes 7 0 [ Array.truncate (valid.Length - 3) valid ] ]) 0 "a truncated rule"
            Expect.equal (accepted [ lookupBytes 7 0 [ corrupt 0 0 valid ] ]) 1 "a format that is none of the six is accepted and applies nothing (src/hb-ot-layout-gsubgpos.hh, Context::sanitize's default)"
            let chain = chain3 [ [ 3 ] ] [ [ 1 ]; [ 2 ] ] [ [ 3 ] ] [ 0, 0 ]
            Expect.equal (accepted [ lookupBytes 8 0 [ chain ] ]) 1 "a valid chained subtable is accepted"
            Expect.equal (accepted [ lookupBytes 8 0 [ corrupt 2 0x7FFF chain ] ]) 0 "a backtrack count that runs past the table"
            Expect.equal (accepted [ lookupBytes 8 0 [ Array.truncate (chain.Length - 5) chain ] ]) 0 "a truncated chained subtable"
            let zeroGlyphs = context3 [] [ 0, 0 ]
            Expect.equal (accepted [ lookupBytes 7 0 [ zeroGlyphs ] ]) 0 "a third-format subtable with no coverage is refused so the first can be read (ContextFormat3_6::sanitize)"
        }

        test "sanitize: an extension wraps a context or chained subtable, never another extension, and its offset is not the Null object" {
            let context = context3 [ [ 1 ]; [ 2 ] ] [ 0, 0 ]
            let accepted (lookups: byte array list) = (loadGpos (tableBytes lookups)).Lookups.Length
            Expect.equal (accepted [ lookupBytes 9 0 [ extension 7 context ] ]) 1 "an extension of a context subtable"
            Expect.equal (accepted [ lookupBytes 9 0 [ extension 8 (chain3 [] [ [ 1 ] ] [] [ 0, 0 ]) ] ]) 1 "an extension of a chained-context subtable"
            Expect.equal (accepted [ lookupBytes 9 0 [ extension 9 context ] ]) 0 "an extension of an extension (ExtensionFormat1::sanitize)"
            let nullOffset = Array.mapi (fun index b -> if index >= 4 && index < 8 then 0uy else b) (extension 7 context)
            Expect.equal (accepted [ lookupBytes 9 0 [ nullOffset ] ]) 0 "a zero inner offset is the Null object, which lies outside the blob (src/hb-open-type.hh:424-428)"
            Expect.equal (accepted [ lookupBytes 9 0 [ extension 7 context; extension 8 (chain3 [] [ [ 1 ] ] [] [ 0, 0 ]) ] ]) 0 "extensions of different types in one lookup"
            let table = loadGpos (tableBytes [ lookupBytes 9 0 [ extension 7 context ] ])
            match table.Lookups[0].Value.Subtables[0].Subtable with
            | PosContext (CoverageRule (coverages, _)) -> Expect.equal coverages.Length 2 "the extension is unwrapped to the context subtable"
            | _ -> failtest "an extension of a context subtable parses as one"
        }

        test "a lookup the gates stop costs no pass and no bytes, and a covered lookup whose rules fail allocates nothing" {
            let neverMatching = lookupBytes 7 0 [ context1 [ 1 ] [ ruleSetBytes [ ruleBytes [ 99 ] [ 0, 0 ] ] ] ]
            let chained = lookupBytes 8 0 [ chain3 [ [ 99 ] ] [ [ 1 ]; [ 2 ] ] [] [ 0, 0 ] ]
            let gatedLookup = lookupBytes 7 0 [ context1 [ 9 ] [ ruleSetBytes [ ruleBytes [ 99 ] [ 0, 0 ] ] ] ]
            let table = loadGpos (tableBytes (singles @ [ neverMatching; chained; gatedLookup ]))
            let struct (maps, stages) = mapFor [ 2 ]
            let gdef = font.Ot.Gdef.Value
            let big (glyph: uint32) = basesOf (Array.create 10000 glyph)
            let measure (lookup: int) (run: GlyphRun) =
                let struct (maps, stages) = mapFor [ lookup ]
                let covered = Engine.gposLookupCoveredOf table maps
                let glyphLookups = Engine.gposGlyphLookupsOf table maps
                let bytes = Allocation.bytesAllocatedAtBest (fun () -> Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run |> ignore |> TailCalls.settle)
                bytes, obj.ReferenceEquals(Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run, run)
            ignore maps
            ignore stages
            let uncovered = big 1u
            let gatedBytes, gatedSame = measure 4 uncovered
            Expect.equal gatedBytes 0L "no glyph of the run is one the lookup starts at: it is not visited"
            Expect.isTrue gatedSame "and the run comes back as it went in"
            let failedBytes, failedSame = measure 2 uncovered
            Expect.equal failedBytes 0L "10000 covered glyphs whose rules all fail allocate nothing"
            Expect.isTrue failedSame "and the run comes back as it went in"
            let chainedBytes, chainedSame = measure 3 (big 1u)
            Expect.equal chainedBytes 0L "10000 covered glyphs whose chained rules fail at the backtrack allocate nothing"
            Expect.isTrue chainedSame "and the run comes back as it went in"
        }

        test "the in-place pair loop refuses a run a context lookup can touch" {
            let pair = lookupBytes 2 0 [ pairBytes 1 1 25 ]
            let context = lookupBytes 7 0 [ context3 [ [ 2 ] ] [ 0, 0 ] ]
            let table = loadGpos (tableBytes [ pair; context ])
            let struct (maps, stages) = mapFor [ 0; 1 ]
            let gdef = font.Ot.Gdef.Value
            let covered = Engine.gposLookupCoveredOf table maps
            let glyphLookups = Engine.gposGlyphLookupsOf table maps
            let pairs = Engine.gposPairsOf table gdef maps stages 1u covered glyphLookups
            let own (glyphs: uint32 array) =
                let infos = ImmutableArray.CreateRange (glyphs |> Array.mapi (fun index glyph -> { Codepoint = glyph; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u }))
                let positions = ImmutableArray.CreateRange (glyphs |> Array.map (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }))
                Engine.positionOwned pairs font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault
            Expect.isTrue (own [| 1u; 1u |] |> ValueOption.isSome) "a run of the pair's glyphs alone is applied in place"
            Expect.isTrue (own [| 1u; 2u |] |> ValueOption.isNone) "a run with a glyph the context lookup starts at is refused"
        }

        test "the kernel equals the literal transliteration of upstream's context apply, on every field of the run" {
            let rnd = Random 0x5C0DE
            for case in 1 .. PropertyCaseCount do
                let table = loadGpos (randomTable rnd)
                Expect.isGreaterThan table.Lookups.Length 2 $"case {case}: the random table is accepted"
                let length = rnd.Next 13
                let glyphs = Array.init length (fun _ -> uint32 (match rnd.Next 9 with | 0 -> 4 | _ -> universe[rnd.Next universe.Length]))
                let ligProps () = match rnd.Next 5 with | 0 -> byte ((1 + rnd.Next 3) <<< 5 ||| rnd.Next 3) | _ -> 0uy
                let mutable cluster = 0u
                let infos =
                    glyphs
                    |> Array.mapi (fun index glyph ->
                        if index > 0 && rnd.Next 3 > 0 then cluster <- cluster + 1u
                        { Codepoint = glyph; Mask = (match rnd.Next 12 with | 0 -> 2u | _ -> 1u); Cluster = cluster; Var1 = 0u; Var2 = 0u })
                let props = glyphs |> Array.map (fun glyph -> { Unchecked.defaultof<GlyphProps> with GlyphClass = classOfGlyph (int glyph); LigProps = ligProps () })
                let positions = glyphs |> Array.map (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
                let run =
                    { GlyphRun.empty with
                        Infos = ImmutableArray.CreateRange infos
                        Props = ImmutableArray.CreateRange props
                        Positions = ImmutableArray.CreateRange positions
                        HavePositions = true
                        ContentType = GlyphContent
                        SegmentProperties = { Direction = (match rnd.Next 4 with | 0 -> Direction.Rtl | _ -> Direction.Ltr); Script = Scripts.Latin; Language = Language.cLocale }
                        Flags = (match rnd.Next 3 with | 0 -> BufferFlags.Default | _ -> BufferFlags.ProduceUnsafeToConcat)
                        ClusterLevel = [| ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters |][rnd.Next 3]
                        MaxOps = (match rnd.Next 8 with | 0 -> rnd.Next 30 | _ -> GlyphRun.MaxOpsDefault) }
                let positioned = positionWith table [ 0 .. table.Lookups.Length - 1 ] run
                if positioned.Positions.AsSpan().ToArray() <> positions then counters[0] <- counters[0] + 1
                if masks positioned |> Array.exists (fun mask -> mask &&& 2u <> 0u) then counters[1] <- counters[1] + 1
                if positioned.MaxOps < run.MaxOps && not positioned.Successful then counters[2] <- counters[2] + 1
            printfn "P5.S12 kernel against the literal transliteration: %d random tables, %d changed a position, %d left an unsafe-to-concat mark, %d failed the buffer" PropertyCaseCount counters[0] counters[1] counters[2]
            Expect.isGreaterThan counters[0] 1500 "many tables move glyphs"
            Expect.isGreaterThan counters[1] 1500 "many tables mark unsafe-to-concat"
        }
    ]
