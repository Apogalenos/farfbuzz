namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The accumulating state of one syllable chunk: its rows and the boundary patches its local merges produced.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal SyllableState =
    { Rows: ImmutableArray<GlyphRow>
      Left: ImmutableArray<BoundaryPatch>
      Right: ImmutableArray<BoundaryPatch> }

/// <summary>Local syllable-chunk edits, local cluster merges, chunking and the dotted-circle pass.</summary>
[<RequireQualifiedAccess>]
module internal SyllableEdit =
    /// <summary>The dotted circle code point, <c>0x25CC</c>.</summary>
    [<Literal>]
    let private DottedCircle = 0x25CCu

    /// <summary>The glyph-flag range above which a flag request is ignored, <c>HB_MAX_GLYPH_FLAG_RANGE</c>.</summary>
    [<Literal>]
    let private MaxGlyphFlagRange = 255

    /// <summary>All defined glyph flags, <c>HB_GLYPH_FLAG_DEFINED</c>.</summary>
    [<Literal>]
    let private GlyphFlagDefined = 7u

    /// <summary>The unsafe-to-break and unsafe-to-concat flags or-ed together.</summary>
    [<Literal>]
    let private UnsafeToBreakMask = 3u

    /// <summary>Whether a cluster level merges into monotone order, <c>HB_BUFFER_CLUSTER_LEVEL_IS_MONOTONE</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True for the two monotone levels.</returns>
    let private isMonotone (level: ClusterLevel) =
        match level with
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters -> true
        | ClusterLevel.Characters | ClusterLevel.Graphemes -> false

    /// <summary>Whether a cluster level is exactly <c>HB_BUFFER_CLUSTER_LEVEL_CHARACTERS</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True only for the characters level.</returns>
    let private isExactlyCharacters (level: ClusterLevel) =
        match level with
        | ClusterLevel.Characters -> true
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters | ClusterLevel.Graphemes -> false

    /// <summary>Finds the smallest cluster over a range.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="best">The smallest cluster so far.</param>
    /// <returns>The smallest cluster.</returns>
    [<TailCall>]
    let rec private minOverRange (rows: ImmutableArray<GlyphRow>) (index: int) (finish: int) (best: uint32) : uint32 =
        match index < finish with
        | false -> best
        | true -> minOverRange rows (index + 1) finish (min best rows[index].Info.Cluster)

    /// <summary>Finds the smallest cluster a flag or merge request treats as its interior, <c>_infos_find_min_cluster</c>.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="level">The cluster level.</param>
    /// <returns>The interior cluster.</returns>
    let private minCluster (rows: ImmutableArray<GlyphRow>) (start: int) (finish: int) (level: ClusterLevel) : uint32 =
        match isExactlyCharacters level with
        | true -> minOverRange rows start finish UInt32.MaxValue
        | false -> min rows[start].Info.Cluster rows[finish - 1].Info.Cluster

    /// <summary>Assigns a cluster to a row, clearing its glyph flags when the cluster changes, <c>set_cluster</c>.</summary>
    /// <param name="row">The row.</param>
    /// <param name="cluster">The cluster.</param>
    /// <returns>The row with the cluster.</returns>
    let private setRowCluster (row: GlyphRow) (cluster: uint32) : GlyphRow =
        match row.Info.Cluster = cluster with
        | true -> row
        | false -> { row with Info = { row.Info with Mask = row.Info.Mask &&& ~~~GlyphFlagDefined; Cluster = cluster } }

    /// <summary>Counts the syllable chunks of a property run.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="start">The next chunk's first index.</param>
    /// <param name="count">The chunks counted so far.</param>
    /// <returns>The chunk count.</returns>
    [<TailCall>]
    let rec private countChunks (props: ReadOnlySpan<GlyphProps>) (start: int) (count: int) : int =
        match start < props.Length with
        | false -> count
        | true -> countChunks props (Sealed.nextSyllableEnd props start) (count + 1)

    /// <summary>Unfolds a property run into its syllable chunks.</summary>
    [<Struct>]
    type private ChunkUnfolder =
        interface ISpanUnfolder<GlyphProps, int, struct (int * int)> with
            member _.Next (props, start) =
                let finish = Sealed.nextSyllableEnd props start
                struct (struct (start, finish), finish)

    /// <summary>Finds the syllable chunks of a run, <c>foreach_syllable</c> with the 64-glyph cap.</summary>
    /// <param name="props">The per-glyph properties, read for their syllable bytes.</param>
    /// <returns>The chunks as ascending [start, end) pairs.</returns>
    let chunks (props: ImmutableArray<GlyphProps>) : ImmutableArray<struct (int * int)> =
        let count = countChunks (props.AsSpan()) 0 0
        Sealed.unfoldImmutable props count 0 (ChunkUnfolder())

    /// <summary>Extends a merge's end over the adjacent equal-cluster run, <c>merge_clusters_impl</c>'s end extension.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="finish">The current end index.</param>
    /// <returns>The extended end index.</returns>
    [<TailCall>]
    let rec private extendEnd (rows: ImmutableArray<GlyphRow>) (finish: int) : int =
        match finish < rows.Length && rows[finish - 1].Info.Cluster = rows[finish].Info.Cluster with
        | true -> extendEnd rows (finish + 1)
        | false -> finish

    /// <summary>Extends a merge's start over the adjacent equal-cluster run, <c>merge_clusters_impl</c>'s start extension.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="start">The current start index.</param>
    /// <returns>The extended start index.</returns>
    [<TailCall>]
    let rec private extendStart (rows: ImmutableArray<GlyphRow>) (start: int) : int =
        match start > 0 && rows[start - 1].Info.Cluster = rows[start].Info.Cluster with
        | true -> extendStart rows (start - 1)
        | false -> start

    /// <summary>Extends a merge's end and, when it reaches the chunk edge, adds the right boundary patch.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="finish">The current end index.</param>
    /// <param name="cluster">The merged cluster.</param>
    /// <param name="patches">The right patches so far.</param>
    /// <returns>The extended end index and the right patches.</returns>
    let private extendEndMerge (rows: ImmutableArray<GlyphRow>) (finish: int) (cluster: uint32) (patches: ImmutableArray<BoundaryPatch>) : struct (int * ImmutableArray<BoundaryPatch>) =
        match cluster = rows[finish - 1].Info.Cluster with
        | true -> struct (finish, patches)
        | false ->
            let extended = extendEnd rows finish
            match extended = rows.Length with
            | true -> struct (rows.Length, Sealed.appendOne (patches.AsSpan()) (BoundaryPatch.MergeRun (rows[rows.Length - 1].Info.Cluster, cluster)))
            | false -> struct (extended, patches)

    /// <summary>Extends a merge's start and, when it reaches the chunk edge, adds the left boundary patch.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="start">The current start index.</param>
    /// <param name="cluster">The merged cluster.</param>
    /// <param name="patches">The left patches so far.</param>
    /// <returns>The extended start index and the left patches.</returns>
    let private extendStartMerge (rows: ImmutableArray<GlyphRow>) (start: int) (cluster: uint32) (patches: ImmutableArray<BoundaryPatch>) : struct (int * ImmutableArray<BoundaryPatch>) =
        match cluster = rows[start].Info.Cluster with
        | true -> struct (start, patches)
        | false ->
            let extended = extendStart rows start
            match extended = 0 with
            | true -> struct (0, Sealed.appendOne (patches.AsSpan()) (BoundaryPatch.MergeRun (rows[0].Info.Cluster, cluster)))
            | false -> struct (extended, patches)

    /// <summary>Flags a row unsafe to break and concat.</summary>
    /// <param name="row">The row.</param>
    /// <returns>The flagged row.</returns>
    let private flagRow (row: GlyphRow) : GlyphRow =
        { row with Info = { row.Info with Mask = row.Info.Mask ||| UnsafeToBreakMask } }

    /// <summary>Flags every row of a range.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="fromIndex">The first flagged index.</param>
    /// <param name="toIndex">The end flagged index.</param>
    /// <returns>The rows with the range flagged.</returns>
    let private flagRange (rows: ImmutableArray<GlyphRow>) (fromIndex: int) (toIndex: int) : ImmutableArray<GlyphRow> =
        Sealed.mapi
            (fun i row ->
                match i >= fromIndex && i < toIndex with
                | true -> flagRow row
                | false -> row)
            (rows.AsSpan())

    /// <summary>Finds the first index of the maximal suffix whose cluster differs from <paramref name="clusterFirst"/>.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="start">The range start.</param>
    /// <param name="index">The candidate boundary, moving left.</param>
    /// <param name="clusterFirst">The boundary cluster.</param>
    /// <returns>The suffix start.</returns>
    [<TailCall>]
    let rec private walkBack (rows: ImmutableArray<GlyphRow>) (start: int) (index: int) (clusterFirst: uint32) : int =
        match index > start && rows[index - 1].Info.Cluster <> clusterFirst with
        | true -> walkBack rows start (index - 1) clusterFirst
        | false -> index

    /// <summary>Finds the end index of the maximal prefix whose cluster differs from <paramref name="clusterLast"/>.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="finish">The range end.</param>
    /// <param name="index">The candidate boundary, moving right.</param>
    /// <param name="clusterLast">The boundary cluster.</param>
    /// <returns>The prefix end.</returns>
    [<TailCall>]
    let rec private walkForward (rows: ImmutableArray<GlyphRow>) (finish: int) (index: int) (clusterLast: uint32) : int =
        match index < finish && rows[index].Info.Cluster <> clusterLast with
        | true -> walkForward rows finish (index + 1) clusterLast
        | false -> index

    /// <summary>Marks a range unsafe to break, skipping its minimum cluster, <c>unsafe_to_break</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="state">The chunk state.</param>
    /// <returns>The chunk state with the flags applied.</returns>
    let unsafeToBreak (level: ClusterLevel) (start: int) (finish: int) (state: SyllableState) : SyllableState =
        match finish - start > MaxGlyphFlagRange || finish - start < 2 with
        | true -> state
        | false ->
            let rows = state.Rows
            let cluster = minCluster rows start finish level
            let clusterFirst = rows[start].Info.Cluster
            let clusterLast = rows[finish - 1].Info.Cluster
            let rows2 =
                match isExactlyCharacters level || (cluster <> clusterFirst && cluster <> clusterLast) with
                | true ->
                    Sealed.mapi
                        (fun i row ->
                            match i >= start && i < finish && row.Info.Cluster <> cluster with
                            | true -> flagRow row
                            | false -> row)
                        (rows.AsSpan())
                | false ->
                    match cluster = clusterFirst with
                    | true -> flagRange rows (walkBack rows start finish clusterFirst) finish
                    | false -> flagRange rows start (walkForward rows finish start clusterLast)
            { state with Rows = rows2 }

    /// <summary>Merges clusters over a range of a chunk, emitting a boundary patch when the extension reaches a chunk edge, <c>merge_clusters</c> + <c>merge_clusters_impl</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="state">The chunk state.</param>
    /// <returns>The chunk state with the clusters merged.</returns>
    let mergeClusters (level: ClusterLevel) (start: int) (finish: int) (state: SyllableState) : SyllableState =
        match finish - start < 2 with
        | true -> state
        | false ->
            match isMonotone level with
            | false -> unsafeToBreak level start finish state
            | true ->
                let rows = state.Rows
                let cluster = minOverRange rows start finish UInt32.MaxValue
                let struct (finish2, right) = extendEndMerge rows finish cluster state.Right
                let struct (start2, left) = extendStartMerge rows start cluster state.Left
                let rows2 =
                    Sealed.mapi
                        (fun i row ->
                            match i >= start2 && i < finish2 with
                            | true -> setRowCluster row cluster
                            | false -> row)
                        (rows.AsSpan())
                { Rows = rows2
                  Left = left
                  Right = right }

    /// <summary>Finds the leftmost insertion position of the row being inserted among the sorted prefix.</summary>
    /// <param name="key">The sort key.</param>
    /// <param name="start">The sort range start.</param>
    /// <param name="rows">The rows.</param>
    /// <param name="j">The candidate insertion position, moving left.</param>
    /// <param name="itemKey">The inserted row's key.</param>
    /// <returns>The insertion position.</returns>
    [<TailCall>]
    let rec private walkInsertion (key: GlyphRow -> int) (start: int) (rows: ImmutableArray<GlyphRow>) (j: int) (itemKey: int) : int =
        match j > start && key rows[j - 1] > itemKey with
        | true -> walkInsertion key start rows (j - 1) itemKey
        | false -> j

    /// <summary>Walks the insertion sort of <see cref="sortMerging"/>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="key">The sort key.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="i">The next row to insert.</param>
    /// <param name="state">The chunk state.</param>
    /// <returns>The chunk state after inserting rows start + 1 through finish.</returns>
    [<TailCall>]
    let rec private sortMergingLoop (level: ClusterLevel) (key: GlyphRow -> int) (start: int) (finish: int) (i: int) (state: SyllableState) : SyllableState =
        match i < finish with
        | false -> state
        | true ->
            let j = walkInsertion key start state.Rows i (key state.Rows[i])
            match j = i with
            | true -> sortMergingLoop level key start finish (i + 1) state
            | false ->
                let merged = mergeClusters level j (i + 1) state
                let rows = Sealed.move i j merged.Rows
                sortMergingLoop level key start finish (i + 1) { merged with Rows = rows }

    /// <summary>Sorts a range of rows, merging clusters of each moved span, <c>hb_buffer_t::sort</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="key">The sort key.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="state">The chunk state.</param>
    /// <returns>The chunk state with the range sorted.</returns>
    let sortMerging (level: ClusterLevel) (key: GlyphRow -> int) (start: int) (finish: int) (state: SyllableState) : SyllableState =
        sortMergingLoop level key start finish (start + 1) state

    /// <summary>The phases of the dotted-circle pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DottedPhase =
        | Open
        | Scan
        | SkipRepha
        | Done

    /// <summary>The state of the dotted-circle pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DottedState =
        { Phase: DottedPhase
          LastSyllable: byte
          Pending: struct (GlyphInfo * GlyphProps) voption }

    /// <summary>Builds the zeroed properties of the inserted dotted circle.</summary>
    /// <param name="syllable">The broken syllable byte.</param>
    /// <param name="category">The dotted-circle category.</param>
    /// <param name="position">The dotted-circle position, zero when none.</param>
    /// <returns>The properties.</returns>
    let private dottedProps (syllable: byte) (category: byte) (position: byte) : GlyphProps =
        { UnicodeProps = 0us
          GlyphClass = 0us
          LigProps = 0uy
          Syllable = syllable
          ShaperCategory = category
          ShaperAuxiliary = position
          GlyphIndex = 0u }

    /// <summary>The dotted-circle insertion pass, <c>hb_syllabic_insert_dotted_circles</c>.</summary>
    [<Struct>]
    type private DottedPass =
        val Glyph: uint32
        val BrokenType: byte
        val Category: byte
        val Position: byte
        val Repha: byte voption
        new (glyph, brokenType, category, position, repha) =
            { Glyph = glyph
              BrokenType = brokenType
              Category = category
              Position = position
              Repha = repha }

        interface IRunPass<DottedState> with
            member this.Step (cursor, state) =
                match state.Phase with
                | DottedPhase.Open -> struct (RunEdit.ClearOutput, { state with Phase = DottedPhase.Scan })
                | DottedPhase.Scan ->
                    let hasInput = cursor.Index < cursor.Input.Length && cursor.Successful
                    let syllable = (match hasInput with | true -> cursor.InputProps[cursor.Index].Syllable | false -> 0uy)
                    let cur = (match hasInput with | true -> cursor.Input[cursor.Index] | false -> Unchecked.defaultof<GlyphInfo>)
                    let broken = hasInput && state.LastSyllable <> syllable && (syllable &&& 0x0Fuy) = this.BrokenType
                    let curCategory = (match hasInput with | true -> cursor.InputProps[cursor.Index].ShaperCategory | false -> 0uy)
                    let info = { Codepoint = this.Glyph; Mask = cur.Mask; Cluster = cur.Cluster; Var1 = 0u; Var2 = 0u }
                    let props = dottedProps syllable this.Category this.Position
                    let skipRepha = (match broken, this.Repha with | true, ValueSome repha -> curCategory = repha | _ -> false)
                    match struct (hasInput, broken, skipRepha) with
                    | struct (false, _, _) -> struct (RunEdit.Finish, { state with Phase = DottedPhase.Done })
                    | struct (true, false, _) -> struct (RunEdit.NextGlyph, state)
                    | struct (true, true, false) -> struct (RunEdit.OutputInfo (info, props), { state with LastSyllable = syllable })
                    | struct (true, true, true) -> struct (RunEdit.NextGlyph, { state with LastSyllable = syllable; Pending = ValueSome (struct (info, props)); Phase = DottedPhase.SkipRepha })
                | DottedPhase.SkipRepha ->
                    let isRepha =
                        match this.Repha with
                        | ValueSome repha -> cursor.Index < cursor.Input.Length && cursor.Successful && cursor.InputProps[cursor.Index].Syllable = state.LastSyllable && cursor.InputProps[cursor.Index].ShaperCategory = repha
                        | ValueNone -> false
                    match state.Pending with
                    | ValueNone -> struct (RunEdit.Finish, { state with Phase = DottedPhase.Done })
                    | ValueSome (struct (info, props)) ->
                        match isRepha with
                        | true -> struct (RunEdit.NextGlyph, state)
                        | false -> struct (RunEdit.OutputInfo (info, props), { state with Phase = DottedPhase.Scan; Pending = ValueNone })
                | DottedPhase.Done -> struct (RunEdit.Finish, state)

    /// <summary>Inserts a dotted circle before every broken syllable, after its leading repha glyphs, <c>hb_syllabic_insert_dotted_circles</c>.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="brokenType">The broken syllable type.</param>
    /// <param name="category">The dotted-circle category.</param>
    /// <param name="repha">The repha category to skip, or none.</param>
    /// <param name="position">The dotted-circle position, or none.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with dotted circles inserted, and whether the pass ran.</returns>
    let insertDottedCircles (font: Font) (brokenType: byte) (category: byte) (repha: byte voption) (position: byte voption) (run: GlyphRun) : struct (GlyphRun * bool) =
        match run.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default with
        | false -> struct (run, false)
        | true ->
            match run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable = ScratchFlags.Default with
            | true -> struct (run, false)
            | false ->
                match Font.nominalGlyph font DottedCircle with
                | ValueNone -> struct (run, false)
                | ValueSome glyph ->
                    let positionByte = match position with | ValueSome p -> p | ValueNone -> 0uy
                    let seed = { Phase = DottedPhase.Open; LastSyllable = 0uy; Pending = ValueNone }
                    let result = GlyphRun.runPass (DottedPass(glyph, brokenType, category, positionByte, repha)) seed run
                    struct (result, true)

    /// <summary>Inserts a dotted circle before every broken syllable in a working set through the pass of the out-buffer on a run, <see cref="insertDottedCircles"/>.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="brokenType">The broken syllable type.</param>
    /// <param name="category">The dotted-circle category.</param>
    /// <param name="repha">The repha category to skip, or none.</param>
    /// <param name="position">The dotted-circle position, or none.</param>
    /// <param name="run">The working set.</param>
    /// <returns>The set with dotted circles inserted, and whether the pass ran.</returns>
    let private insertedByPass (font: Font) (brokenType: byte) (category: byte) (repha: byte voption) (position: byte voption) (run: WorkingSet) =
        let struct (inserted, ran) = insertDottedCircles font brokenType category repha position (GlyphRun.runOf run)
        struct (GlyphRun.setOf inserted, ran)

    /// <summary>Inserts a dotted circle before every broken syllable as <see cref="insertDottedCircles"/> does, for a run whose arrays
    /// were made in this call: the glyphs are counted by one scan and written once into arrays of their size, where the out-buffer pass
    /// builds its own. A run that did not succeed takes <see cref="insertDottedCircles"/>.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="brokenType">The broken syllable type.</param>
    /// <param name="category">The dotted-circle category.</param>
    /// <param name="repha">The repha category to skip, or none.</param>
    /// <param name="position">The dotted-circle position, or none.</param>
    /// <param name="run">The working set, with owned arrays; a set the circles would make longer than its length limit takes the pass of the
    /// out-buffer, which fails the buffer where the limit is crossed.</param>
    /// <returns>The set with dotted circles inserted, and whether the pass ran.</returns>
    let insertDottedCirclesOwned (font: Font) (brokenType: byte) (category: byte) (repha: byte voption) (position: byte voption) (run: WorkingSet) =
        let enabled = run.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default && run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable <> ScratchFlags.Default
        let glyph =
            match enabled with
            | true -> Font.nominalGlyph font DottedCircle
            | false -> ValueNone
        match run.Successful, enabled, glyph with
        | false, _, _ -> insertedByPass font brokenType category repha position run
        | true, false, _ | true, true, ValueNone -> struct (run, false)
        | true, true, ValueSome circle ->
            let positionByte = match position with | ValueSome p -> p | ValueNone -> 0uy
            let hasRepha, rephaByte = match repha with | ValueSome r -> true, r | ValueNone -> false, 0uy
            let struct (infos, props) = SyllableOwned.insertDottedCircles run.Infos run.Props circle brokenType category positionByte hasRepha rephaByte
            match infos.Length > run.MaxLen with
            | true -> insertedByPass font brokenType category repha position run
            | false -> struct ({ run with Infos = infos; Props = props }, true)
