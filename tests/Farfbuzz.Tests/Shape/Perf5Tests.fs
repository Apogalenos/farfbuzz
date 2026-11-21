module Farfbuzz.Tests.Shape.Perf5Tests

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz

/// <summary>One edit a shaping pass applies to the out-buffer machine.</summary>
[<Struct>]
type internal OldEdit =
    | ClearOutput
    | NextGlyph
    | NextGlyphs of count: int
    | ReplaceGlyph of replacement: uint32
    | ReplaceGlyphProps of codepoint: uint32 * props: GlyphProps
    | ReplaceGlyphs of consumed: int * produced: ImmutableArray<uint32>
    | OutputGlyph of output: uint32
    | OutputInfo of info: GlyphInfo * props: GlyphProps
    | CopyGlyph
    | SkipGlyph
    | DeleteGlyph
    | MoveTo of target: int
    | MergeClusters of mergeStart: int * mergeEnd: int
    | MergeGraphemeClusters of graphemeStart: int * graphemeEnd: int
    | MergeOutClusters of outStart: int * outEnd: int
    | UnsafeToBreak of breakStart: int * breakEnd: int
    | UnsafeToConcat of concatStart: int * concatEnd: int
    | UnsafeToBreakFromOutbuffer of obStart: int * obEnd: int
    | UnsafeToConcatFromOutbuffer of ocStart: int * ocEnd: int
    | SafeToInsertTatweel of tatweelStart: int * tatweelEnd: int
    | SetMasks of value: uint32 * mask: uint32 * clusterStart: uint32 * clusterEnd: uint32
    | OrMask of mask: uint32 * start: int * finish: int
    | SetCurrentProps of current: GlyphProps
    | SetOutputGlyph of outputIndex: int * codepoint: uint32 * outputProps: GlyphProps
    | PopOutput
    | Sync
    | Finish
    | SetInputProps of inputIndex: int * inputProps: GlyphProps
    | SetPosition of positionIndex: int * position: GlyphPosition
    | SetIndex of index: int
    | SetSerial of serial: byte
    | SetRandomState of randomState: uint32
    | AddScratch of scratch: ScratchFlags
    | ChargeOps of opsCharged: int
    | Fail
    | SetInputGlyph of inputIndex: int * codepoint: uint32 * inputProps: GlyphProps
    | Rearrange of rearrangeStart: int * rearrangeEnd: int * moveLeft: int * moveRight: int * flipLeft: bool * flipRight: bool

/// <summary>The unsafe-to-break glyph flag, <c>HB_GLYPH_FLAG_UNSAFE_TO_BREAK</c>.</summary>
[<Literal>]
let private GlyphFlagUnsafeToBreak = 1u

/// <summary>The unsafe-to-concat glyph flag, <c>HB_GLYPH_FLAG_UNSAFE_TO_CONCAT</c>.</summary>
[<Literal>]
let private GlyphFlagUnsafeToConcat = 2u

/// <summary>The safe-to-insert-tatweel glyph flag, <c>HB_GLYPH_FLAG_SAFE_TO_INSERT_TATWEEL</c>.</summary>
[<Literal>]
let private GlyphFlagSafeToInsertTatweel = 4u

/// <summary>All defined glyph flags, <c>HB_GLYPH_FLAG_DEFINED</c>.</summary>
[<Literal>]
let private GlyphFlagDefined = 7u

/// <summary>The growth increment of the out-buffer, the <c>+ 32</c> in <c>enlarge</c>.</summary>
[<Literal>]
let private BufferGrowIncrement = 32

/// <summary>The glyph-flag range limit above which a flag request is ignored.</summary>
[<Literal>]
let private MaxGlyphFlagRange = 255

/// <summary>The idle default maximum buffer length, <c>HB_BUFFER_MAX_LEN_DEFAULT</c>.</summary>
[<Literal>]
let private DefaultMaxLen = 0x3FFFFFFF

/// <summary>The machine state of one shaping pass over the out-buffer. A reference type with mutable fields:
/// the run-pass interpreter mutates it in place, so a step copies no machine state.</summary>
type internal OldMachine =
    { mutable Info: GlyphInfo array
      mutable OutInfo: GlyphInfo array
      mutable Props: GlyphProps array
      mutable OutProps: GlyphProps array
      mutable Positions: GlyphPosition array
      SourceInfos: ImmutableArray<GlyphInfo>
      SourceProps: ImmutableArray<GlyphProps>
      SourcePositions: ImmutableArray<GlyphPosition>
      mutable Materialized: bool
      mutable PositionsMaterialized: bool
      mutable OutIsSeparate: bool
      mutable Idx: int
      mutable Len: int
      mutable OutLen: int
      mutable HaveOutput: bool
      mutable HavePositions: bool
      mutable MaxOps: int
      mutable Successful: bool
      mutable Passive: bool
      MaxLen: int
      ClusterLevel: ClusterLevel
      Flags: BufferFlags
      mutable Serial: byte
      mutable RandomState: uint32
      mutable ScratchFlags: ScratchFlags
      InitialMaxOps: int }

/// <summary>The logical out-buffer array, <c>out_info</c> (aliases <c>info</c> until it outgrows it).</summary>
/// <param name="machine">The machine.</param>
/// <returns>The out-buffer array.</returns>
let private outInfoArray (machine: OldMachine) : GlyphInfo array =
    match machine.OutIsSeparate with
    | true -> machine.OutInfo
    | false -> machine.Info

/// <summary>The properties array parallel to <see cref="outInfoArray"/>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The out-buffer properties array.</returns>
let private outPropsArray (machine: OldMachine) : GlyphProps array =
    match machine.OutIsSeparate with
    | true -> machine.OutProps
    | false -> machine.Props

/// <summary>Whether a cluster level merges into monotone order, <c>HB_BUFFER_CLUSTER_LEVEL_IS_MONOTONE</c>.</summary>
/// <param name="level">The cluster level.</param>
/// <returns>True for the two monotone levels.</returns>
let private isMonotone (level: ClusterLevel) =
    match level with
    | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters -> true
    | ClusterLevel.Characters | ClusterLevel.Graphemes -> false

/// <summary>Whether a cluster level groups by graphemes, <c>HB_BUFFER_CLUSTER_LEVEL_IS_GRAPHEMES</c>.</summary>
/// <param name="level">The cluster level.</param>
/// <returns>True for the two grapheme levels.</returns>
let private isGraphemes (level: ClusterLevel) =
    match level with
    | ClusterLevel.MonotoneGraphemes | ClusterLevel.Graphemes -> true
    | ClusterLevel.MonotoneCharacters | ClusterLevel.Characters -> false

/// <summary>Whether a cluster level is exactly <c>HB_BUFFER_CLUSTER_LEVEL_CHARACTERS</c>, as the flag functions test.</summary>
/// <param name="level">The cluster level.</param>
/// <returns>True only for the characters level.</returns>
let private isExactlyCharacters (level: ClusterLevel) =
    match level with
    | ClusterLevel.Characters -> true
    | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters | ClusterLevel.Graphemes -> false

/// <summary>Grows an allocation until it is at least <paramref name="size"/>, <c>enlarge</c>.</summary>
/// <param name="size">The required size.</param>
/// <param name="allocated">The current allocation.</param>
/// <returns>The new allocation.</returns>
[<TailCall>]
let rec private growAllocated (size: int) (allocated: int) : int =
    match size < allocated with
    | true -> allocated
    | false -> growAllocated size (allocated + (allocated >>> 1) + BufferGrowIncrement)

/// <summary>Copies the input glyphs and properties into fresh working arrays on the first edit that changes the run.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with its own glyph and properties arrays.</returns>
let private materialize (machine: OldMachine) : OldMachine =
    match machine.Materialized with
    | true -> machine
    | false ->
        let allocated = match machine.Len = 0 with | true -> 0 | false -> growAllocated machine.Len 0
        let info = Array.zeroCreate allocated
        let props = Array.zeroCreate allocated
        match machine.SourceInfos.IsEmpty with
        | true -> ()
        | false -> machine.SourceInfos.AsSpan(0, machine.Len).CopyTo(Span<GlyphInfo>(info))
        match machine.SourceProps.IsEmpty with
        | true -> ()
        | false -> machine.SourceProps.AsSpan(0, machine.Len).CopyTo(Span<GlyphProps>(props))
        machine.Info <- info
        machine.Props <- props
        machine.Materialized <- true
        machine

/// <summary>Copies the input positions into a fresh array on the first position edit.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with its own positions array.</returns>
let private materializePositions (machine: OldMachine) : OldMachine =
    match machine.PositionsMaterialized with
    | true -> machine
    | false ->
        let positions = Array.zeroCreate machine.Info.Length
        match machine.HavePositions && not machine.SourcePositions.IsEmpty with
        | true -> machine.SourcePositions.AsSpan(0, machine.Len).CopyTo(Span<GlyphPosition>(positions))
        | false -> ()
        machine.Positions <- positions
        machine.PositionsMaterialized <- true
        machine

/// <summary>Allocates the separate out-buffer arrays when the output outgrows the input, <c>make_room_for</c>'s copy.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with out-buffer arrays large enough for the current input.</returns>
let private ensureOutArrays (machine: OldMachine) : OldMachine =
    match machine.OutInfo.Length >= machine.Info.Length with
    | true -> machine
    | false ->
        machine.OutInfo <- Array.zeroCreate machine.Info.Length
        machine.OutProps <- Array.zeroCreate machine.Info.Length
        machine

/// <summary>Reallocates the machine arrays to fit at least <paramref name="size"/> items.</summary>
/// <param name="machine">The machine.</param>
/// <param name="size">The required size.</param>
/// <returns>The machine with larger arrays, or a failed machine when the size exceeds <c>MaxLen</c>.</returns>
let private enlarge (machine: OldMachine) (size: int) : OldMachine =
    match size > machine.MaxLen with
    | true ->
        machine.Successful <- false
        machine
    | false ->
        match machine.Successful with
        | false -> machine
        | true ->
            let newAllocated = growAllocated size machine.Info.Length
            let info = Array.zeroCreate newAllocated
            let outInfo = Array.zeroCreate newAllocated
            let props = Array.zeroCreate newAllocated
            let outProps = Array.zeroCreate newAllocated
            let positions = match machine.Positions.Length with | 0 -> Array.zeroCreate 0 | _ -> Array.zeroCreate newAllocated
            Array.Copy(machine.Info, info, machine.Info.Length)
            Array.Copy(machine.OutInfo, outInfo, machine.OutInfo.Length)
            Array.Copy(machine.Props, props, machine.Props.Length)
            Array.Copy(machine.OutProps, outProps, machine.OutProps.Length)
            Array.Copy(machine.Positions, positions, machine.Positions.Length)
            machine.Info <- info
            machine.OutInfo <- outInfo
            machine.Props <- props
            machine.OutProps <- outProps
            machine.Positions <- positions
            machine

/// <summary>Ensures the arrays hold at least <paramref name="size"/> items, <c>ensure</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="size">The required size.</param>
/// <returns>The machine, enlarged when needed, and whether the size is now available.</returns>
let private ensure (machine: OldMachine) (size: int) : struct (OldMachine * bool) =
    match size = 0 || size < machine.Info.Length with
    | true -> struct (machine, true)
    | false ->
        let enlarged = enlarge machine size
        struct (enlarged, enlarged.Successful)

/// <summary>Ensures room for an output, switching the out-buffer to a separate array when needed, <c>make_room_for</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="numIn">The number of input glyphs consumed.</param>
/// <param name="numOut">The number of output glyphs produced.</param>
/// <returns>The machine and whether the room is available.</returns>
let private makeRoomFor (machine: OldMachine) (numIn: int) (numOut: int) : struct (OldMachine * bool) =
    let struct (machine, ok) = ensure machine (machine.OutLen + numOut)
    match ok with
    | false -> struct (machine, false)
    | true ->
        match (not machine.OutIsSeparate) && machine.OutLen + numOut > machine.Idx + numIn with
        | false -> struct (machine, true)
        | true ->
            let machine = ensureOutArrays machine
            Array.Copy(machine.Info, 0, machine.OutInfo, 0, machine.OutLen)
            Array.Copy(machine.Props, 0, machine.OutProps, 0, machine.OutLen)
            machine.OutIsSeparate <- true
            struct (machine, true)

/// <summary>Assigns a cluster to a glyph, clearing its flags when the cluster changes, <c>set_cluster</c>.</summary>
/// <param name="info">The glyph.</param>
/// <param name="cluster">The cluster.</param>
/// <param name="mask">The mask whose defined bits replace the flags.</param>
/// <returns>The glyph with the cluster.</returns>
let private setClusterValue (info: GlyphInfo) (cluster: uint32) (mask: uint32) : GlyphInfo =
    match info.Cluster = cluster with
    | true -> info
    | false -> { info with Mask = (info.Mask &&& ~~~GlyphFlagDefined) ||| (mask &&& GlyphFlagDefined); Cluster = cluster }

/// <summary>Copies the current glyph, or the previous output glyph at end of input, <c>cur</c>/<c>prev</c>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The glyph and its properties.</returns>
let private curOrPrev (machine: OldMachine) : struct (GlyphInfo * GlyphProps) =
    match machine.Idx < machine.Len with
    | true -> struct (machine.Info[machine.Idx], machine.Props[machine.Idx])
    | false ->
        let outInfo = outInfoArray machine
        let outProps = outPropsArray machine
        let index = match machine.OutLen > 0 with | true -> machine.OutLen - 1 | false -> 0
        struct (outInfo[index], outProps[index])

/// <summary>Finds the smallest cluster in a range, <c>_infos_find_min_cluster</c>.</summary>
/// <param name="infos">The glyphs.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <param name="cluster">The starting minimum.</param>
/// <param name="level">The cluster level.</param>
/// <returns>The minimum cluster.</returns>
let private infosFindMinCluster (infos: GlyphInfo array) (start: int) (finish: int) (cluster: uint32) (level: ClusterLevel) : uint32 =
    match start = finish with
    | true -> cluster
    | false ->
        match isExactlyCharacters level with
        | true ->
            let mutable minimum = cluster
            let mutable i = start
            while i < finish do
                minimum <- min minimum infos[i].Cluster
                i <- i + 1
            minimum
        | false -> min cluster (min infos[start].Cluster infos[finish - 1].Cluster)

/// <summary>Ors a mask into the glyphs of a range whose cluster differs, <c>_infos_set_glyph_flags</c>.</summary>
/// <param name="infos">The glyphs.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <param name="cluster">The cluster to keep unset.</param>
/// <param name="mask">The mask to or in.</param>
/// <param name="level">The cluster level.</param>
/// <param name="maxOps">The operation budget.</param>
/// <param name="successful">Whether the machine has not failed.</param>
/// <returns>The updated budget and success flag.</returns>
let private infosSetGlyphFlags (infos: GlyphInfo array) (start: int) (finish: int) (cluster: uint32) (mask: uint32) (level: ClusterLevel) (maxOps: int) (successful: bool) : struct (int * bool) =
    match start = finish with
    | true -> struct (maxOps, successful)
    | false ->
        let maxOps = maxOps - (finish - start)
        let successful = match maxOps < 0 with | true -> false | false -> successful
        let clusterFirst = infos[start].Cluster
        let clusterLast = infos[finish - 1].Cluster
        match isExactlyCharacters level || (cluster <> clusterFirst && cluster <> clusterLast) with
        | true ->
            let mutable i = start
            while i < finish do
                match cluster = infos[i].Cluster with
                | false -> infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                | true -> ()
                i <- i + 1
            struct (maxOps, successful)
        | false ->
            match cluster = clusterFirst with
            | true ->
                let mutable i = finish
                while start < i && infos[i - 1].Cluster <> clusterFirst do
                    i <- i - 1
                    infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                struct (maxOps, successful)
            | false ->
                let mutable i = start
                while i < finish && infos[i].Cluster <> clusterLast do
                    infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                    i <- i + 1
                struct (maxOps, successful)

/// <summary>Applies a glyph-flag request to the input or output buffer, <c>_set_glyph_flags_impl</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="mask">The mask to or in.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <param name="interior">Whether the minimum cluster is skipped.</param>
/// <param name="fromOutBuffer">Whether the range is in the output buffer.</param>
/// <returns>The machine with the flags applied.</returns>
let private setGlyphFlagsImpl (machine: OldMachine) (mask: uint32) (start: int) (finish: int) (interior: bool) (fromOutBuffer: bool) : OldMachine =
    match (not fromOutBuffer) || (not machine.HaveOutput) with
    | true ->
        match interior with
        | false ->
            let mutable i = start
            while i < finish do
                machine.Info[i] <- { machine.Info[i] with Mask = machine.Info[i].Mask ||| mask }
                i <- i + 1
            machine
        | true ->
            let cluster = infosFindMinCluster machine.Info start finish UInt32.MaxValue machine.ClusterLevel
            let struct (maxOps, successful) = infosSetGlyphFlags machine.Info start finish cluster mask machine.ClusterLevel machine.MaxOps machine.Successful
            machine.MaxOps <- maxOps
            machine.Successful <- successful
            machine
    | false ->
        let outInfo = outInfoArray machine
        match interior with
        | false ->
            let mutable i = start
            while i < machine.OutLen do
                outInfo[i] <- { outInfo[i] with Mask = outInfo[i].Mask ||| mask }
                i <- i + 1
            let mutable j = machine.Idx
            while j < finish do
                machine.Info[j] <- { machine.Info[j] with Mask = machine.Info[j].Mask ||| mask }
                j <- j + 1
            machine
        | true ->
            let cluster = infosFindMinCluster machine.Info machine.Idx finish UInt32.MaxValue machine.ClusterLevel
            let cluster = infosFindMinCluster outInfo start machine.OutLen cluster machine.ClusterLevel
            let struct (maxOps, successful) = infosSetGlyphFlags outInfo start machine.OutLen cluster mask machine.ClusterLevel machine.MaxOps machine.Successful
            let struct (maxOps2, successful2) = infosSetGlyphFlags machine.Info machine.Idx finish cluster mask machine.ClusterLevel maxOps successful
            machine.MaxOps <- maxOps2
            machine.Successful <- successful2
            machine

/// <summary>Ors a glyph flag into a range, honouring the range limit, <c>_set_glyph_flags</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="mask">The mask to or in.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index, or -1 for the end of the buffer.</param>
/// <param name="interior">Whether the minimum cluster is skipped.</param>
/// <param name="fromOutBuffer">Whether the range is in the output buffer.</param>
/// <returns>The machine with the flags applied.</returns>
let private setGlyphFlags (machine: OldMachine) (mask: uint32) (start: int) (finish: int) (interior: bool) (fromOutBuffer: bool) : OldMachine =
    match finish <> -1 && (uint32 finish - uint32 start) > uint32 MaxGlyphFlagRange with
    | true -> machine
    | false ->
        let finish = match finish with | -1 -> machine.Len | bounded -> min bounded machine.Len
        match interior && (not fromOutBuffer) && finish - start < 2 with
        | true -> machine
        | false -> setGlyphFlagsImpl machine mask start finish interior fromOutBuffer

/// <summary>Marks a range unsafe to break and concat, <c>unsafe_to_break</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the flags applied.</returns>
let private unsafeToBreak (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    setGlyphFlags machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) start finish true false

/// <summary>Marks a range safe to insert a tatweel, <c>safe_to_insert_tatweel</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the flags applied.</returns>
let private safeToInsertTatweel (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match (machine.Flags &&& BufferFlags.ProduceSafeToInsertTatweel) = BufferFlags.Default with
    | true -> unsafeToBreak machine start finish
    | false -> setGlyphFlags machine GlyphFlagSafeToInsertTatweel start finish true false

/// <summary>Marks a range unsafe to concat, <c>unsafe_to_concat</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the flags applied.</returns>
let private unsafeToConcat (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
    | true -> machine
    | false -> setGlyphFlags machine GlyphFlagUnsafeToConcat start finish false false

/// <summary>Marks an output-buffer range unsafe to break, <c>unsafe_to_break_from_outbuffer</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the flags applied.</returns>
let private unsafeToBreakFromOutbuffer (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    setGlyphFlags machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) start finish true true

/// <summary>Marks an output-buffer range unsafe to concat, <c>unsafe_to_concat_from_outbuffer</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the flags applied.</returns>
let private unsafeToConcatFromOutbuffer (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
    | true -> machine
    | false -> setGlyphFlags machine GlyphFlagUnsafeToConcat start finish false true

/// <summary>Copies glyphs from input to output, advancing the cursor, <c>next_glyphs</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="count">The number of glyphs.</param>
/// <returns>The machine with the glyphs moved.</returns>
let private nextGlyphs (machine: OldMachine) (count: int) : OldMachine =
    match machine.HaveOutput with
    | false ->
        machine.Idx <- machine.Idx + count
        machine
    | true ->
        match machine.OutIsSeparate || machine.OutLen <> machine.Idx with
        | false ->
            machine.Idx <- machine.Idx + count
            machine.OutLen <- machine.OutLen + count
            machine
        | true ->
            let struct (machine, ok) = ensure machine (machine.OutLen + count)
            match ok with
            | false -> machine
            | true ->
                let outInfo = outInfoArray machine
                let outProps = outPropsArray machine
                Array.Copy(machine.Info, machine.Idx, outInfo, machine.OutLen, count)
                Array.Copy(machine.Props, machine.Idx, outProps, machine.OutLen, count)
                machine.Idx <- machine.Idx + count
                machine.OutLen <- machine.OutLen + count
                machine

/// <summary>Merges clusters over a range, <c>merge_clusters_impl</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="startIn">The first index.</param>
/// <param name="finishIn">The end index.</param>
/// <returns>The machine with the clusters merged.</returns>
let private mergeClustersImpl (machine: OldMachine) (startIn: int) (finishIn: int) : OldMachine =
    let mutable start = startIn
    let mutable finish = finishIn
    let mutable maxOps = machine.MaxOps
    let mutable successful = machine.Successful
    let mutable cluster = machine.Info[start].Cluster
    let mutable i = start + 1
    while i < finish do
        cluster <- min cluster machine.Info[i].Cluster
        i <- i + 1
    match successful with
    | true ->
        match cluster = machine.Info[finish - 1].Cluster with
        | false ->
            while finish < machine.Len && machine.Info[finish - 1].Cluster = machine.Info[finish].Cluster do
                finish <- finish + 1
        | true -> ()
        match cluster = machine.Info[start].Cluster with
        | false ->
            while machine.Idx < start && machine.Info[start - 1].Cluster = machine.Info[start].Cluster do
                start <- start - 1
        | true -> ()
        match machine.Idx = start && machine.Info[start].Cluster <> cluster with
        | true ->
            let outInfo = outInfoArray machine
            let mutable j = machine.OutLen
            while j > 0 && outInfo[j - 1].Cluster = machine.Info[start].Cluster do
                j <- j - 1
                outInfo[j] <- setClusterValue outInfo[j] cluster 0u
            maxOps <- maxOps - (machine.OutLen - j)
        | false -> ()
    | false -> ()
    maxOps <- maxOps - (finish - start)
    match maxOps < 0 with
    | true -> successful <- false
    | false -> ()
    let mutable k = start
    while k < finish do
        machine.Info[k] <- setClusterValue machine.Info[k] cluster 0u
        k <- k + 1
    machine.MaxOps <- maxOps
    machine.Successful <- successful
    machine

/// <summary>Merges clusters in monotone levels, <c>merge_clusters</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the clusters merged.</returns>
let private mergeClusters (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match finish - start < 2 with
    | true -> machine
    | false ->
        match isMonotone machine.ClusterLevel with
        | false -> unsafeToBreak machine start finish
        | true -> mergeClustersImpl machine start finish

/// <summary>Merges grapheme clusters in grapheme levels, <c>merge_grapheme_clusters</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the clusters merged.</returns>
let private mergeGraphemeClusters (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match finish - start < 2 with
    | true -> machine
    | false ->
        match isGraphemes machine.ClusterLevel with
        | false -> unsafeToBreak machine start finish
        | true -> mergeClustersImpl machine start finish

/// <summary>Merges clusters over a range of the output buffer, <c>merge_out_clusters_impl</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="startIn">The first index.</param>
/// <param name="finishIn">The end index.</param>
/// <returns>The machine with the clusters merged.</returns>
let private mergeOutClustersImpl (machine: OldMachine) (startIn: int) (finishIn: int) : OldMachine =
    let outInfo = outInfoArray machine
    let mutable start = startIn
    let mutable finish = finishIn
    let mutable maxOps = machine.MaxOps
    let mutable successful = machine.Successful
    let mutable cluster = outInfo[start].Cluster
    let mutable i = start + 1
    while i < finish do
        cluster <- min cluster outInfo[i].Cluster
        i <- i + 1
    match successful with
    | true ->
        match cluster = outInfo[start].Cluster with
        | false ->
            while start > 0 && outInfo[start - 1].Cluster = outInfo[start].Cluster do
                start <- start - 1
        | true -> ()
        match cluster = outInfo[finish - 1].Cluster with
        | false ->
            while finish < machine.OutLen && outInfo[finish - 1].Cluster = outInfo[finish].Cluster do
                finish <- finish + 1
        | true -> ()
        match finish = machine.OutLen && outInfo[finish - 1].Cluster <> cluster with
        | true ->
            let mutable j = machine.Idx
            while j < machine.Len && machine.Info[j].Cluster = outInfo[finish - 1].Cluster do
                machine.Info[j] <- setClusterValue machine.Info[j] cluster 0u
                j <- j + 1
            maxOps <- maxOps - (j - machine.Idx)
        | false -> ()
    | false -> ()
    maxOps <- maxOps - (finish - start)
    match maxOps < 0 with
    | true -> successful <- false
    | false -> ()
    let mutable k = start
    while k < finish do
        outInfo[k] <- setClusterValue outInfo[k] cluster 0u
        k <- k + 1
    machine.MaxOps <- maxOps
    machine.Successful <- successful
    machine

/// <summary>Merges output clusters in monotone levels, <c>merge_out_clusters</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the clusters merged.</returns>
let private mergeOutClusters (machine: OldMachine) (start: int) (finish: int) : OldMachine =
    match finish - start < 2 || not (isMonotone machine.ClusterLevel) with
    | true -> machine
    | false -> mergeOutClustersImpl machine start finish

/// <summary>Rewrites one input glyph into the output, <c>replace_glyphs</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="numIn">The number of input glyphs consumed.</param>
/// <param name="numOut">The number of output glyphs produced.</param>
/// <param name="glyphs">The produced code points.</param>
/// <returns>The machine with the glyphs replaced.</returns>
let private replaceGlyphs (machine: OldMachine) (numIn: int) (numOut: int) (glyphs: ReadOnlySpan<uint32>) : OldMachine =
    let struct (machine, ok) = makeRoomFor machine numIn numOut
    match ok with
    | false -> machine
    | true ->
        let machine = mergeClusters machine machine.Idx (machine.Idx + numIn)
        let struct (origInfo, origProps) = curOrPrev machine
        let outInfo = outInfoArray machine
        let outProps = outPropsArray machine
        let mutable i = 0
        while i < numOut do
            outInfo[machine.OutLen + i] <- { origInfo with Codepoint = glyphs[i] }
            outProps[machine.OutLen + i] <- origProps
            i <- i + 1
        machine.Idx <- machine.Idx + numIn
        machine.OutLen <- machine.OutLen + numOut
        machine

/// <summary>Rewrites the current glyph's code point and properties into the output, consuming one glyph,
/// <c>replace_glyph</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="codepoint">The replacement code point.</param>
/// <param name="props">The replacement properties.</param>
/// <returns>The machine with the glyph replaced.</returns>
let private replaceGlyph (machine: OldMachine) (codepoint: uint32) (props: GlyphProps) : OldMachine =
    let struct (machine, ok) = makeRoomFor machine 1 1
    match ok with
    | false -> machine
    | true ->
        let origInfo = machine.Info[machine.Idx]
        let outInfo = outInfoArray machine
        let outProps = outPropsArray machine
        outInfo[machine.OutLen] <- { origInfo with Codepoint = codepoint }
        outProps[machine.OutLen] <- props
        machine.Idx <- machine.Idx + 1
        machine.OutLen <- machine.OutLen + 1
        machine

/// <summary>Appends a whole glyph to the output, <c>output_info</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="info">The glyph.</param>
/// <param name="props">Its properties.</param>
/// <returns>The machine with the glyph appended.</returns>
let private outputInfo (machine: OldMachine) (info: GlyphInfo) (props: GlyphProps) : OldMachine =
    let struct (machine, ok) = makeRoomFor machine 0 1
    match ok with
    | false -> machine
    | true ->
        let outInfo = outInfoArray machine
        let outProps = outPropsArray machine
        outInfo[machine.OutLen] <- info
        outProps[machine.OutLen] <- props
        machine.OutLen <- machine.OutLen + 1
        machine

/// <summary>Merges the deleted glyph's cluster backward over the output, <c>delete_glyph</c> helper.</summary>
/// <param name="machine">The machine.</param>
/// <param name="cluster">The deleted glyph's cluster.</param>
/// <returns>The machine with the cluster merged backward.</returns>
let private mergeClusterBackward (machine: OldMachine) (cluster: uint32) : OldMachine =
    let outInfo = outInfoArray machine
    match cluster < outInfo[machine.OutLen - 1].Cluster && machine.Successful with
    | false -> machine
    | true ->
        let mask = machine.Info[machine.Idx].Mask
        let oldCluster = outInfo[machine.OutLen - 1].Cluster
        let mutable i = machine.OutLen
        while i > 0 && outInfo[i - 1].Cluster = oldCluster do
            i <- i - 1
            outInfo[i] <- setClusterValue outInfo[i] cluster mask
        let maxOps = machine.MaxOps - (machine.OutLen - i)
        machine.MaxOps <- maxOps
        machine.Successful <- match maxOps < 0 with | true -> false | false -> machine.Successful
        machine

/// <summary>Deletes the current glyph, merging its cluster, <c>delete_glyph</c>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with the glyph skipped.</returns>
let private deleteGlyph (machine: OldMachine) : OldMachine =
    let cluster = machine.Info[machine.Idx].Cluster
    let outInfo = outInfoArray machine
    let survives =
        (machine.Idx + 1 < machine.Len && cluster = machine.Info[machine.Idx + 1].Cluster)
        || (machine.OutLen > 0 && cluster = outInfo[machine.OutLen - 1].Cluster)
    match survives with
    | true ->
        machine.Idx <- machine.Idx + 1
        machine
    | false ->
        match machine.OutLen > 0 with
        | true ->
            let machine = mergeClusterBackward machine cluster
            machine.Idx <- machine.Idx + 1
            machine
        | false ->
            match machine.Idx + 1 < machine.Len with
            | true ->
                let machine = mergeClusters machine machine.Idx (machine.Idx + 2)
                machine.Idx <- machine.Idx + 1
                machine
            | false ->
                machine.Idx <- machine.Idx + 1
                machine

/// <summary>Shifts the remaining input forward, <c>shift_forward</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="count">The number of slots to shift.</param>
/// <returns>The machine with the input shifted.</returns>
let private shiftForward (machine: OldMachine) (count: int) : OldMachine =
    let struct (machine, ok) = ensure machine (machine.Len + count)
    match ok with
    | false -> machine
    | true ->
        let maxOps = machine.MaxOps - (machine.Len - machine.Idx)
        match maxOps < 0 with
        | true ->
            machine.MaxOps <- maxOps
            machine.Successful <- false
            machine
        | false ->
            Array.Copy(machine.Info, machine.Idx, machine.Info, machine.Idx + count, machine.Len - machine.Idx)
            Array.Copy(machine.Props, machine.Idx, machine.Props, machine.Idx + count, machine.Len - machine.Idx)
            match machine.Idx + count > machine.Len with
            | true ->
                let mutable i = machine.Len
                while i < machine.Idx + count do
                    machine.Info[i] <- Unchecked.defaultof<GlyphInfo>
                    machine.Props[i] <- Unchecked.defaultof<GlyphProps>
                    i <- i + 1
                machine.MaxOps <- maxOps
                machine.Len <- machine.Len + count
                machine.Idx <- machine.Idx + count
                machine
            | false ->
                machine.MaxOps <- maxOps
                machine.Len <- machine.Len + count
                machine.Idx <- machine.Idx + count
                machine

/// <summary>Moves the cursor forward over the output, <c>move_to</c> helper.</summary>
/// <param name="machine">The machine.</param>
/// <param name="count">The number of glyphs to advance.</param>
/// <returns>The machine with the cursor moved forward.</returns>
let private moveToForward (machine: OldMachine) (count: int) : OldMachine =
    let struct (machine, ok) = makeRoomFor machine count count
    match ok with
    | false -> machine
    | true ->
        let maxOps = machine.MaxOps - count
        match maxOps < 0 with
        | true ->
            machine.MaxOps <- maxOps
            machine.Successful <- false
            machine
        | false ->
            let outInfo = outInfoArray machine
            let outProps = outPropsArray machine
            Array.Copy(machine.Info, machine.Idx, outInfo, machine.OutLen, count)
            Array.Copy(machine.Props, machine.Idx, outProps, machine.OutLen, count)
            machine.MaxOps <- maxOps
            machine.Idx <- machine.Idx + count
            machine.OutLen <- machine.OutLen + count
            machine

/// <summary>Rewinds the cursor, moving output glyphs back to the input, <c>move_to</c> helper.</summary>
/// <param name="machine">The machine.</param>
/// <param name="count">The number of glyphs to rewind.</param>
/// <returns>The machine with the cursor rewound.</returns>
let private moveToRewind (machine: OldMachine) (count: int) : OldMachine =
    let maxOps = machine.MaxOps - count
    match maxOps < 0 with
    | true ->
        machine.MaxOps <- maxOps
        machine.Successful <- false
        machine
    | false ->
        let idx = machine.Idx - count
        let outLen = machine.OutLen - count
        let outInfo = outInfoArray machine
        let outProps = outPropsArray machine
        Array.Copy(outInfo, outLen, machine.Info, idx, count)
        Array.Copy(outProps, outLen, machine.Props, idx, count)
        machine.MaxOps <- maxOps
        machine.Idx <- idx
        machine.OutLen <- outLen
        machine

/// <summary>Rewinds the cursor, shifting forward first when needed, <c>move_to</c> helper.</summary>
/// <param name="machine">The machine.</param>
/// <param name="count">The number of glyphs to rewind.</param>
/// <returns>The machine with the cursor rewound.</returns>
let private moveToRewindChecked (machine: OldMachine) (count: int) : OldMachine =
    match machine.Idx < count with
    | true ->
        let machine = shiftForward machine (count - machine.Idx)
        match machine.Successful with
        | false -> machine
        | true -> moveToRewind machine count
    | false -> moveToRewind machine count

/// <summary>Moves the cursor to an output index, <c>move_to</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="target">The target output index.</param>
/// <returns>The machine with the cursor moved.</returns>
let private moveTo (machine: OldMachine) (target: int) : OldMachine =
    match machine.HaveOutput with
    | false ->
        machine.Idx <- target
        machine
    | true ->
        match machine.Successful with
        | false -> machine
        | true ->
            match machine.OutLen < target, machine.OutLen > target with
            | true, _ -> moveToForward machine (target - machine.OutLen)
            | false, false -> machine
            | false, true -> moveToRewindChecked machine (machine.OutLen - target)

/// <summary>Rewrites glyph masks, <c>set_masks</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="value">The mask value to set.</param>
/// <param name="mask">The mask selecting the bits.</param>
/// <param name="clusterStart">The first cluster to match.</param>
/// <param name="clusterEnd">The cluster after the last to match.</param>
/// <returns>The machine with the masks set.</returns>
let private setMasks (machine: OldMachine) (value: uint32) (mask: uint32) (clusterStart: uint32) (clusterEnd: uint32) : OldMachine =
    match mask = 0u with
    | true -> machine
    | false ->
        let notMask = ~~~mask
        let value = value &&& mask
        let maxOps = machine.MaxOps - machine.Len
        let successful = match maxOps < 0 with | true -> false | false -> machine.Successful
        match clusterStart = 0u && clusterEnd = UInt32.MaxValue with
        | true ->
            let mutable i = 0
            while i < machine.Len do
                machine.Info[i] <- { machine.Info[i] with Mask = (machine.Info[i].Mask &&& notMask) ||| value }
                i <- i + 1
            machine.MaxOps <- maxOps
            machine.Successful <- successful
            machine
        | false ->
            let mutable i = 0
            while i < machine.Len do
                match clusterStart <= machine.Info[i].Cluster && machine.Info[i].Cluster < clusterEnd with
                | true -> machine.Info[i] <- { machine.Info[i] with Mask = (machine.Info[i].Mask &&& notMask) ||| value }
                | false -> ()
                i <- i + 1
            machine.MaxOps <- maxOps
            machine.Successful <- successful
            machine

/// <summary>Ors a mask into an index range, the fraction <c>info[j].mask |= mask</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="mask">The mask to or in.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The machine with the masks applied.</returns>
let private orMask (machine: OldMachine) (mask: uint32) (start: int) (finish: int) : OldMachine =
    let mutable i = start
    while i < finish do
        machine.Info[i] <- { machine.Info[i] with Mask = machine.Info[i].Mask ||| mask }
        i <- i + 1
    machine

/// <summary>Rewrites the properties of the current input glyph, the first half of <c>next_char</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="props">The properties.</param>
/// <returns>The machine with the current glyph's properties rewritten.</returns>
let private setCurrentProps (machine: OldMachine) (props: GlyphProps) : OldMachine =
    machine.Props[machine.Idx] <- props
    machine

/// <summary>Rewrites one output glyph's code point and properties, keeping its cluster and mask from the merge,
/// <c>hb-ot-shape-normalize.cc</c>'s starter rewrite.</summary>
/// <param name="machine">The machine.</param>
/// <param name="index">The output index.</param>
/// <param name="codepoint">The composed code point.</param>
/// <param name="props">The glyph properties.</param>
/// <returns>The machine with the output glyph rewritten.</returns>
let private setOutputGlyph (machine: OldMachine) (index: int) (codepoint: uint32) (props: GlyphProps) : OldMachine =
    let outInfo = outInfoArray machine
    let outProps = outPropsArray machine
    outInfo[index] <- { outInfo[index] with Codepoint = codepoint }
    outProps[index] <- props
    machine

/// <summary>Removes the last output glyph, the <c>out_len--</c> of the compose round.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with one fewer output glyph.</returns>
let private popOutput (machine: OldMachine) : OldMachine =
    machine.OutLen <- machine.OutLen - 1
    machine

/// <summary>Rewrites one input glyph's properties in place, without moving the cursor.</summary>
/// <param name="machine">The machine.</param>
/// <param name="inputIndex">The input index.</param>
/// <param name="inputProps">The properties.</param>
/// <returns>The machine with the glyph's properties rewritten.</returns>
let private setInputProps (machine: OldMachine) (inputIndex: int) (inputProps: GlyphProps) : OldMachine =
    machine.Props[inputIndex] <- inputProps
    machine

/// <summary>Rewrites one glyph position in place, without moving the cursor.</summary>
/// <param name="machine">The machine.</param>
/// <param name="positionIndex">The position index.</param>
/// <param name="position">The position.</param>
/// <returns>The machine with the position rewritten.</returns>
let private setPosition (machine: OldMachine) (positionIndex: int) (position: GlyphPosition) : OldMachine =
    let machine = materializePositions machine
    machine.Positions[positionIndex] <- position
    machine

/// <summary>Rewinds the cursor to an index, only when no output buffer is open.</summary>
/// <param name="machine">The machine.</param>
/// <param name="index">The target input index.</param>
/// <returns>The machine with the cursor rewound.</returns>
let private setIndex (machine: OldMachine) (index: int) : OldMachine =
    match machine.HaveOutput with
    | true -> machine
    | false ->
        machine.Idx <- index
        machine

/// <summary>Rewrites the buffer serial, <c>hb_buffer_t::serial</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="serial">The new serial.</param>
/// <returns>The machine with the serial rewritten.</returns>
let private setSerial (machine: OldMachine) (serial: byte) : OldMachine =
    machine.Serial <- serial
    machine

/// <summary>Rewrites the buffer random state, <c>hb_buffer_t::random_state</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="randomState">The new random state.</param>
/// <returns>The machine with the random state rewritten.</returns>
let private setRandomState (machine: OldMachine) (randomState: uint32) : OldMachine =
    machine.RandomState <- randomState
    machine

/// <summary>Ors a scratch flag into the buffer.</summary>
/// <param name="machine">The machine.</param>
/// <param name="scratch">The flag to add.</param>
/// <returns>The machine with the flag accumulated.</returns>
let private addScratch (machine: OldMachine) (scratch: ScratchFlags) : OldMachine =
    machine.ScratchFlags <- machine.ScratchFlags ||| scratch
    machine

/// <summary>The longest range a rearrangement subtable may span, <c>HB_MAX_CONTEXT_LENGTH</c>.</summary>
[<Literal>]
let private MaxContextLength = 64

/// <summary>Charges operations against the buffer budget, the <c>max_ops -= n</c> of an AAT subtable.</summary>
/// <param name="machine">The machine.</param>
/// <param name="opsCharged">The number of operations to charge.</param>
/// <returns>The machine with the budget reduced.</returns>
let private chargeOps (machine: OldMachine) (opsCharged: int) : OldMachine =
    machine.MaxOps <- machine.MaxOps - opsCharged
    machine

/// <summary>Fails the buffer, <c>buffer->successful = false</c>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine marked unsuccessful.</returns>
let private fail (machine: OldMachine) : OldMachine =
    machine.Successful <- false
    machine

/// <summary>Rewrites one input glyph's code point and properties in place, without moving the cursor,
/// <c>replace_glyph_inplace</c>.</summary>
/// <param name="machine">The machine.</param>
/// <param name="inputIndex">The input index.</param>
/// <param name="codepoint">The replacement code point.</param>
/// <param name="inputProps">The replacement properties.</param>
/// <returns>The machine with the glyph rewritten.</returns>
let private setInputGlyph (machine: OldMachine) (inputIndex: int) (codepoint: uint32) (inputProps: GlyphProps) : OldMachine =
    machine.Info[inputIndex] <- { machine.Info[inputIndex] with Codepoint = codepoint }
    machine.Props[inputIndex] <- inputProps
    machine

/// <summary>Rearranges a range between two moved sides, the copies of <c>RearrangementSubtable::transition</c>
/// (src/hb-aat-layout-morx-table.hh:136-159). Properties move with their infos; positions do not.</summary>
/// <param name="machine">The machine.</param>
/// <param name="start">The first index of the range.</param>
/// <param name="finish">The end index of the range.</param>
/// <param name="moveLeft">The number of glyphs moved from the left side.</param>
/// <param name="moveRight">The number of glyphs moved from the right side.</param>
/// <param name="flipLeft">Whether the moved left pair is flipped.</param>
/// <param name="flipRight">Whether the moved right pair is flipped.</param>
/// <returns>The machine with the range rearranged.</returns>
let private rearrange (machine: OldMachine) (start: int) (finish: int) (moveLeft: int) (moveRight: int) (flipLeft: bool) (flipRight: bool) : OldMachine =
    match finish - start >= moveLeft + moveRight && finish - start <= MaxContextLength with
    | false -> machine
    | true ->
        let infos = machine.Info
        let props = machine.Props
        let leftInfos = infos.AsSpan(start, moveLeft).ToArray()
        let leftProps = props.AsSpan(start, moveLeft).ToArray()
        let rightInfos = infos.AsSpan(finish - moveRight, moveRight).ToArray()
        let rightProps = props.AsSpan(finish - moveRight, moveRight).ToArray()
        match moveLeft = moveRight with
        | false ->
            Array.Copy(infos, start + moveLeft, infos, start + moveRight, finish - start - moveLeft - moveRight)
            Array.Copy(props, start + moveLeft, props, start + moveRight, finish - start - moveLeft - moveRight)
        | true -> ()
        rightInfos.CopyTo(Span<GlyphInfo>(infos, start, moveRight))
        rightProps.CopyTo(Span<GlyphProps>(props, start, moveRight))
        leftInfos.CopyTo(Span<GlyphInfo>(infos, finish - moveLeft, moveLeft))
        leftProps.CopyTo(Span<GlyphProps>(props, finish - moveLeft, moveLeft))
        match flipLeft with
        | true ->
            let temporaryInfo = infos[finish - 1]
            infos[finish - 1] <- infos[finish - 2]
            infos[finish - 2] <- temporaryInfo
            let temporaryProps = props[finish - 1]
            props[finish - 1] <- props[finish - 2]
            props[finish - 2] <- temporaryProps
        | false -> ()
        match flipRight with
        | true ->
            let temporaryInfo = infos[start]
            infos[start] <- infos[start + 1]
            infos[start + 1] <- temporaryInfo
            let temporaryProps = props[start]
            props[start] <- props[start + 1]
            props[start + 1] <- temporaryProps
        | false -> ()
        machine

/// <summary>Opens the out-buffer and clears positions, <c>clear_output</c>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with an open out-buffer.</returns>
let private clearOutput (machine: OldMachine) : OldMachine =
    machine.HaveOutput <- true
    machine.HavePositions <- false
    machine.Idx <- 0
    machine.OutLen <- 0
    machine.OutIsSeparate <- false
    machine

/// <summary>Resets the output-buffer indices after a failed or completed sync.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with the output reset.</returns>
let private syncReset (machine: OldMachine) : OldMachine =
    machine.HaveOutput <- false
    machine.OutLen <- 0
    machine.Idx <- 0
    machine.OutIsSeparate <- false
    machine

/// <summary>Swaps the output into the input after a successful sync.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with the output finalised.</returns>
let private syncFinalize (machine: OldMachine) : OldMachine =
    let finalLen = machine.OutLen
    match machine.OutIsSeparate with
    | false ->
        machine.Len <- finalLen
        machine.HaveOutput <- false
        machine.OutLen <- 0
        machine.Idx <- 0
        machine
    | true ->
        let info = machine.OutInfo
        let outInfo = machine.Info
        let props = machine.OutProps
        let outProps = machine.Props
        machine.Info <- info
        machine.OutInfo <- outInfo
        machine.Props <- props
        machine.OutProps <- outProps
        machine.Len <- finalLen
        machine.HaveOutput <- false
        machine.OutLen <- 0
        machine.Idx <- 0
        machine.OutIsSeparate <- false
        machine

/// <summary>Finalises the output buffer, <c>sync</c>.</summary>
/// <param name="machine">The machine.</param>
/// <returns>The machine with the output finalised.</returns>
let private sync (machine: OldMachine) : OldMachine =
    match machine.HaveOutput with
    | false -> machine
    | true ->
        match machine.Successful with
        | false -> syncReset machine
        | true ->
            let machine = nextGlyphs machine (machine.Len - machine.Idx)
            match machine.Successful with
            | false -> syncReset machine
            | true -> syncFinalize machine

/// <summary>Applies one edit to the machine, tracking the passive-edit flag.</summary>
/// <param name="machine">The machine.</param>
/// <param name="edit">The edit.</param>
/// <returns>The machine after the edit.</returns>
let private applyEdit (machine: OldMachine) (edit: OldEdit) : OldMachine =
    let machine =
        match edit with
        | OldEdit.NextGlyph | OldEdit.NextGlyphs _ | OldEdit.Sync | OldEdit.Finish -> machine
        | _ -> materialize machine
    let applied =
        match edit with
        | OldEdit.ClearOutput -> clearOutput machine
        | OldEdit.NextGlyph -> nextGlyphs machine 1
        | OldEdit.NextGlyphs count -> nextGlyphs machine count
        | OldEdit.ReplaceGlyph replacement -> replaceGlyphs machine 1 1 (ReadOnlySpan [| replacement |])
        | OldEdit.ReplaceGlyphProps (codepoint, props) -> replaceGlyph machine codepoint props
        | OldEdit.ReplaceGlyphs (consumed, produced) -> replaceGlyphs machine consumed produced.Length (produced.AsSpan())
        | OldEdit.OutputGlyph output -> replaceGlyphs machine 0 1 (ReadOnlySpan [| output |])
        | OldEdit.OutputInfo (info, props) -> outputInfo machine info props
        | OldEdit.CopyGlyph -> outputInfo machine machine.Info[machine.Idx] machine.Props[machine.Idx]
        | OldEdit.SkipGlyph ->
            machine.Idx <- machine.Idx + 1
            machine
        | OldEdit.DeleteGlyph -> deleteGlyph machine
        | OldEdit.MoveTo target -> moveTo machine target
        | OldEdit.MergeClusters (s, e) -> mergeClusters machine s e
        | OldEdit.MergeGraphemeClusters (s, e) -> mergeGraphemeClusters machine s e
        | OldEdit.MergeOutClusters (s, e) -> mergeOutClusters machine s e
        | OldEdit.UnsafeToBreak (s, e) -> unsafeToBreak machine s e
        | OldEdit.UnsafeToConcat (s, e) -> unsafeToConcat machine s e
        | OldEdit.UnsafeToBreakFromOutbuffer (s, e) -> unsafeToBreakFromOutbuffer machine s e
        | OldEdit.UnsafeToConcatFromOutbuffer (s, e) -> unsafeToConcatFromOutbuffer machine s e
        | OldEdit.SafeToInsertTatweel (s, e) -> safeToInsertTatweel machine s e
        | OldEdit.SetMasks (value, mask, cs, ce) -> setMasks machine value mask cs ce
        | OldEdit.OrMask (mask, start, finish) -> orMask machine mask start finish
        | OldEdit.SetCurrentProps props -> setCurrentProps machine props
        | OldEdit.SetOutputGlyph (index, codepoint, props) -> setOutputGlyph machine index codepoint props
        | OldEdit.PopOutput -> popOutput machine
        | OldEdit.Sync -> sync machine
        | OldEdit.Finish -> sync machine
        | OldEdit.SetInputProps (index, props) -> setInputProps machine index props
        | OldEdit.SetPosition (index, position) -> setPosition machine index position
        | OldEdit.SetIndex index -> setIndex machine index
        | OldEdit.SetSerial serial -> setSerial machine serial
        | OldEdit.SetRandomState randomState -> setRandomState machine randomState
        | OldEdit.AddScratch scratch -> addScratch machine scratch
        | OldEdit.ChargeOps n -> chargeOps machine n
        | OldEdit.Fail -> fail machine
        | OldEdit.SetInputGlyph (index, codepoint, props) -> setInputGlyph machine index codepoint props
        | OldEdit.Rearrange (start, finish, l, r, flipLeft, flipRight) -> rearrange machine start finish l r flipLeft flipRight
    match edit with
    | OldEdit.NextGlyph | OldEdit.NextGlyphs _ | OldEdit.Sync | OldEdit.Finish -> applied
    | _ ->
        applied.Passive <- false
        applied


/// <summary>Converts a reference (old-union) edit to the current struct edit.</summary>
let private toNew (edit: OldEdit) : RunEdit =
    match edit with
    | OldEdit.ClearOutput -> RunEdit.ClearOutput
    | OldEdit.NextGlyph -> RunEdit.NextGlyph
    | OldEdit.NextGlyphs n -> RunEdit.NextGlyphs n
    | OldEdit.ReplaceGlyph r -> RunEdit.ReplaceGlyph r
    | OldEdit.ReplaceGlyphProps (c, p) -> RunEdit.ReplaceGlyphProps (c, p)
    | OldEdit.ReplaceGlyphs (c, p) -> RunEdit.ReplaceGlyphs (c, p)
    | OldEdit.OutputGlyph o -> RunEdit.OutputGlyph o
    | OldEdit.OutputInfo (i, p) -> RunEdit.OutputInfo (i, p)
    | OldEdit.CopyGlyph -> RunEdit.CopyGlyph
    | OldEdit.SkipGlyph -> RunEdit.SkipGlyph
    | OldEdit.DeleteGlyph -> RunEdit.DeleteGlyph
    | OldEdit.MoveTo t -> RunEdit.MoveTo t
    | OldEdit.MergeClusters (s, e) -> RunEdit.MergeClusters (s, e)
    | OldEdit.MergeGraphemeClusters (s, e) -> RunEdit.MergeGraphemeClusters (s, e)
    | OldEdit.MergeOutClusters (s, e) -> RunEdit.MergeOutClusters (s, e)
    | OldEdit.UnsafeToBreak (s, e) -> RunEdit.UnsafeToBreak (s, e)
    | OldEdit.UnsafeToConcat (s, e) -> RunEdit.UnsafeToConcat (s, e)
    | OldEdit.UnsafeToBreakFromOutbuffer (s, e) -> RunEdit.UnsafeToBreakFromOutbuffer (s, e)
    | OldEdit.UnsafeToConcatFromOutbuffer (s, e) -> RunEdit.UnsafeToConcatFromOutbuffer (s, e)
    | OldEdit.SafeToInsertTatweel (s, e) -> RunEdit.SafeToInsertTatweel (s, e)
    | OldEdit.SetMasks (v, m, cs, ce) -> RunEdit.SetMasks (v, m, cs, ce)
    | OldEdit.OrMask (m, s, f) -> RunEdit.OrMask (m, s, f)
    | OldEdit.SetCurrentProps p -> RunEdit.SetCurrentProps p
    | OldEdit.SetOutputGlyph (i, c, p) -> RunEdit.SetOutputGlyph (i, c, p)
    | OldEdit.PopOutput -> RunEdit.PopOutput
    | OldEdit.Sync -> RunEdit.Sync
    | OldEdit.Finish -> RunEdit.Finish
    | OldEdit.SetInputProps (i, p) -> RunEdit.SetInputProps (i, p)
    | OldEdit.SetPosition (i, p) -> RunEdit.SetPosition (i, p)
    | OldEdit.SetIndex i -> RunEdit.SetIndex i
    | OldEdit.SetSerial s -> RunEdit.SetSerial s
    | OldEdit.SetRandomState r -> RunEdit.SetRandomState r
    | OldEdit.AddScratch s -> RunEdit.AddScratch s
    | OldEdit.ChargeOps n -> RunEdit.ChargeOps n
    | OldEdit.Fail -> RunEdit.Fail
    | OldEdit.SetInputGlyph (i, c, p) -> RunEdit.SetInputGlyph (i, c, p)
    | OldEdit.Rearrange (s, e, l, r, fl, fr) -> RunEdit.Rearrange (s, e, l, r, fl, fr)

/// <summary>Builds the reference machine from the raw run fields, materialised up front with the same headroom
/// <c>Sealed.materialize</c> gives the library machine, so a failed buffer grows (or refuses) identically.</summary>
let private oldMachineOf (infos: GlyphInfo array) (props: GlyphProps array) (positions: GlyphPosition array) (serial: byte) (randomState: uint32) : OldMachine =
    let allocated = growAllocated infos.Length 0
    let info = Array.zeroCreate allocated
    infos.CopyTo(Span<GlyphInfo>(info))
    let propsArray = Array.zeroCreate allocated
    props.CopyTo(Span<GlyphProps>(propsArray))
    { Info = info
      OutInfo = Array.zeroCreate allocated
      Props = propsArray
      OutProps = Array.zeroCreate allocated
      Positions = Array.copy positions
      SourceInfos = ImmutableArray.Empty
      SourceProps = ImmutableArray.Empty
      SourcePositions = ImmutableArray.Empty
      Materialized = true
      PositionsMaterialized = true
      OutIsSeparate = false
      Idx = 0
      Len = infos.Length
      OutLen = 0
      HaveOutput = false
      HavePositions = true
      MaxOps = 0x1FFFFFFF
      Successful = true
      Passive = true
      MaxLen = 0x3FFFFFFF
      ClusterLevel = ClusterLevel.MonotoneGraphemes
      Flags = BufferFlags.Default
      Serial = serial
      RandomState = randomState
      ScratchFlags = ScratchFlags.Default
      InitialMaxOps = 0x1FFFFFFF }

/// <summary>Random glyph properties.</summary>
let private genProps : Gen<GlyphProps> =
    gen {
        let! unicode = Gen.choose (0, 0xFFFF) |> Gen.map uint16
        let! glyphClass = Gen.choose (0, 0xFFFF) |> Gen.map uint16
        let! ligProps = Gen.choose (0, 255) |> Gen.map byte
        let! syllable = Gen.choose (0, 255) |> Gen.map byte
        let! category = Gen.choose (0, 255) |> Gen.map byte
        let! auxiliary = Gen.choose (0, 255) |> Gen.map byte
        let! glyphIndex = Gen.choose (0, 1000) |> Gen.map uint32
        return
            { UnicodeProps = unicode
              GlyphClass = glyphClass
              LigProps = ligProps
              Syllable = syllable
              ShaperCategory = category
              ShaperAuxiliary = auxiliary
              GlyphIndex = glyphIndex }
    }

/// <summary>A random glyph position.</summary>
let private genPosition : Gen<GlyphPosition> =
    gen {
        let! xAdvance = Gen.choose (-1000, 1000)
        let! yAdvance = Gen.choose (-1000, 1000)
        let! xOffset = Gen.choose (-1000, 1000)
        let! yOffset = Gen.choose (-1000, 1000)
        let! var = Gen.choose (0, 1000) |> Gen.map uint32
        return { XAdvance = xAdvance; YAdvance = yAdvance; XOffset = xOffset; YOffset = yOffset; Var = var }
    }

/// <summary>A random half-open range within a length.</summary>
let private genRange (length: int) : Gen<int * int> =
    gen {
        let! start = Gen.choose (0, length)
        let! finish = Gen.choose (start, length)
        return start, finish
    }

/// <summary>An output start and input finish for an out-buffer flag range, <c>unsafe_to_break_from_outbuffer</c>.</summary>
let private genOutFlag (outLen: int) (idx: int) (len: int) : Gen<int * int> =
    gen {
        let! start = Gen.choose (0, outLen)
        let! finish = Gen.choose (idx, len)
        return start, finish
    }

/// <summary>One edit, generated from the reference machine's current state so its arguments stay valid.</summary>
let private genEdit (m: OldMachine) : Gen<OldEdit> =
    let len = m.Len
    let idx = m.Idx
    let outLen = m.OutLen
    let always =
        [ Gen.constant OldEdit.ClearOutput
          Gen.map OldEdit.NextGlyphs (Gen.choose (0, max 0 (len - idx)))
          Gen.map OldEdit.OutputGlyph (Gen.choose (0, 300) |> Gen.map uint32)
          Gen.map OldEdit.SetSerial (Gen.choose (0, 255) |> Gen.map byte)
          Gen.map OldEdit.SetRandomState (Gen.choose (0, 1000) |> Gen.map uint32)
          Gen.map OldEdit.AddScratch (Gen.elements [ ScratchFlags.Default; ScratchFlags.HasCgj; ScratchFlags.HasDefaultIgnorables; ScratchFlags.HasContinuations ])
          Gen.map OldEdit.SetIndex (Gen.choose (0, max 0 (len - 1)))
          Gen.map (fun (s, e) -> OldEdit.OrMask (1u, s, e)) (genRange len)
          Gen.map OldEdit.ChargeOps (Gen.choose (0, 5))
          Gen.constant OldEdit.Fail ]
    let currentGlyph =
        if idx < len then
            [ Gen.constant OldEdit.NextGlyph
              Gen.map OldEdit.ReplaceGlyph (Gen.choose (0, 300) |> Gen.map uint32)
              Gen.map (fun (cp, p) -> OldEdit.ReplaceGlyphProps (cp, p))
                  (Gen.map2 (fun cp p -> cp, p) (Gen.choose (0, 300) |> Gen.map uint32) genProps)
              Gen.constant OldEdit.SkipGlyph
              Gen.constant OldEdit.CopyGlyph
              Gen.constant OldEdit.DeleteGlyph
              Gen.map OldEdit.SetCurrentProps genProps
              Gen.map OldEdit.MoveTo (Gen.choose (0, outLen))
              Gen.map (fun (c, p) -> OldEdit.ReplaceGlyphs (c, p))
                  (gen { let! c = Gen.choose (1, len - idx)
                         let! n = Gen.choose (0, 2)
                         let! produced = Gen.listOfLength n (Gen.choose (0, 300) |> Gen.map uint32)
                         return c, (produced |> List.toArray |> ImmutableArray.CreateRange) }) ]
        else
            []
    let indexed =
        if len = 0 then
            []
        else
            [ Gen.map (fun (s, e) -> OldEdit.MergeClusters (s, e)) (genRange len)
              Gen.map (fun (s, e) -> OldEdit.MergeGraphemeClusters (s, e)) (genRange len)
              Gen.map (fun (s, e) -> OldEdit.UnsafeToBreak (s, e)) (genRange len)
              Gen.map (fun (s, e) -> OldEdit.UnsafeToConcat (s, e)) (genRange len)
              Gen.map (fun (s, e) -> OldEdit.SafeToInsertTatweel (s, e)) (genRange len)
              Gen.map (fun (i, p) -> OldEdit.SetInputProps (i, p)) (Gen.map2 (fun i p -> i, p) (Gen.choose (0, len - 1)) genProps)
              Gen.map (fun (i, p) -> OldEdit.SetPosition (i, p)) (Gen.map2 (fun i p -> i, p) (Gen.choose (0, len - 1)) genPosition)
              Gen.map (fun (i, cp, p) -> OldEdit.SetInputGlyph (i, cp, p))
                  (gen { let! i = Gen.choose (0, len - 1)
                         let! cp = Gen.choose (0, 300) |> Gen.map uint32
                         let! p = genProps
                         return i, cp, p })
              Gen.map (fun (s, e, l, r, fl, fr) -> OldEdit.Rearrange (s, e, l, r, fl, fr))
                  (gen { let! s = Gen.choose (0, len)
                         let! e = Gen.choose (s, len)
                         let! l = Gen.choose (0, 2)
                         let! r = Gen.choose (0, 2)
                         let! flipLeft = Gen.elements [ false; true ]
                         let! flipRight = Gen.elements [ false; true ]
                         return s, e, l, r, flipLeft && l = 2, flipRight && r = 2 })
              Gen.map (fun (v, mask, cs, ce) -> OldEdit.SetMasks (v, mask, cs, ce))
                  (gen { let! v = Gen.choose (0, 255) |> Gen.map uint32
                         let! mask = Gen.choose (0, 255) |> Gen.map uint32
                         let! cs = Gen.choose (0, len) |> Gen.map uint32
                         let! ce = Gen.choose (0, len) |> Gen.map uint32
                         return v, mask, cs, ce }) ]
    let fromOutBuffer =
        if idx < len && outLen > 0 && m.HaveOutput then
            [ Gen.map (fun (s, e) -> OldEdit.MergeOutClusters (s, e)) (genRange outLen)
              Gen.map (fun (s, e) -> OldEdit.UnsafeToBreakFromOutbuffer (s, e)) (genOutFlag outLen idx len)
              Gen.map (fun (s, e) -> OldEdit.UnsafeToConcatFromOutbuffer (s, e)) (genOutFlag outLen idx len) ]
        else
            []
    let output =
        if idx < len then
            let cur = m.Info[idx]
            let outputGlyph =
                if outLen > 0 then
                    [ Gen.map (fun (i, cp, p) -> OldEdit.SetOutputGlyph (i, cp, p))
                          (gen { let! i = Gen.choose (0, outLen - 1)
                                 let! cp = Gen.choose (0, 300) |> Gen.map uint32
                                 let! p = genProps
                                 return i, cp, p })
                      Gen.constant OldEdit.PopOutput ]
                else
                    []
            [ Gen.map (fun (cp, p) -> OldEdit.OutputInfo ({ Codepoint = cp; Mask = cur.Mask; Cluster = cur.Cluster; Var1 = 0u; Var2 = 0u }, p))
                  (Gen.map2 (fun cp p -> cp, p) (Gen.choose (0, 300) |> Gen.map uint32) genProps) ]
            @ outputGlyph
        else
            []
    Gen.oneof (always @ currentGlyph @ indexed @ fromOutBuffer @ output)

/// <summary>Generates a random valid edit sequence, advancing the reference machine as it goes.</summary>
let rec private genEdits (m: OldMachine) (remaining: int) : Gen<OldEdit list> =
    match remaining = 0 with
    | true -> Gen.constant []
    | false ->
        gen {
            let! edit = genEdit m
            applyEdit m edit |> ignore
            let! rest = genEdits m (remaining - 1)
            return edit :: rest
        }

/// <summary>Generates a run, a reference machine and a random valid edit sequence.</summary>
let private genCase : Gen<struct (GlyphRun * OldEdit list)> =
    gen {
        let! length = Gen.choose (1, 6)
        let! cps = Gen.listOfLength length (Gen.choose (0, 300) |> Gen.map uint32)
        let! props = Gen.listOfLength length genProps
        let! positions = Gen.listOfLength length genPosition
        let! serial = Gen.choose (0, 255) |> Gen.map byte
        let! randomState = Gen.choose (0, 1000) |> Gen.map uint32
        let infos = cps |> List.mapi (fun i c -> { Codepoint = c; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u }) |> Array.ofList
        let run =
            { GlyphRun.empty with
                Infos = Sealed.ofSpan (ReadOnlySpan infos)
                Props = Sealed.ofSpan (ReadOnlySpan (Array.ofList props))
                Positions = Sealed.ofSpan (ReadOnlySpan (Array.ofList positions))
                HavePositions = true
                Serial = uint32 serial
                RandomState = randomState }
        let m = oldMachineOf infos (Array.ofList props) (Array.ofList positions) serial randomState
        let! editCount = Gen.choose (1, 8)
        let! edits = genEdits m editCount
        return struct (run, edits)
    }

/// <summary>A pass that replays a script of struct edits.</summary>
[<Struct>]
type private ReplayPass =
    val Edits: RunEdit array
    new (edits) = { Edits = edits }
    interface IRunPass<int> with
        member this.Step (_, index) =
            match index < this.Edits.Length with
            | true -> struct (this.Edits[index], index + 1)
            | false -> struct (RunEdit.Finish, index)

/// <summary>Whether the new machine's result equals the reference machine's final state.</summary>
let private sameResult (result: GlyphRun) (m: OldMachine) : bool =
    result.Infos.AsSpan().SequenceEqual(ReadOnlySpan(m.Info, 0, m.Len))
    && result.Props.AsSpan().SequenceEqual(ReadOnlySpan(m.Props, 0, m.Len))
    && (match m.HavePositions with
        | true -> result.Positions.AsSpan().SequenceEqual(ReadOnlySpan(m.Positions, 0, m.Len))
        | false -> result.Positions.IsEmpty)
    && result.HavePositions = m.HavePositions
    && result.MaxOps = m.MaxOps
    && result.Successful = m.Successful
    && result.Serial = uint32 m.Serial
    && result.RandomState = m.RandomState
    && result.ScratchFlags = m.ScratchFlags

/// <summary>A pass whose every step advances one glyph and changes nothing.</summary>
[<Struct>]
type private NoEditPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.Index < cursor.Input.Length with
            | true -> struct (RunEdit.NextGlyph, ())
            | false -> struct (RunEdit.Finish, ())

/// <summary>A pass that sets the position of every glyph, one per step.</summary>
[<Struct>]
type private SetPositionPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true ->
                struct (RunEdit.SetPosition (state, { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that outputs every glyph through <see cref="RunEdit.OutputInfo"/>, one per step.</summary>
[<Struct>]
type private OutputInfoPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true ->
                struct (RunEdit.OutputInfo (cursor.Input[state], Unchecked.defaultof<GlyphProps>), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that sets the position of one glyph, then finishes.</summary>
[<Struct>]
type private SetOnePositionPass =
    interface IRunPass<int> with
        member _.Step (_, state) =
            match state = 0 with
            | true -> struct (RunEdit.SetPosition (0, { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }), 1)
            | false -> struct (RunEdit.Finish, 1)

/// <summary>A pass that outputs one glyph through <see cref="RunEdit.OutputInfo"/>, then finishes.</summary>
[<Struct>]
type private OutputOneInfoPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state = 0 with
            | true -> struct (RunEdit.OutputInfo (cursor.Input[0], Unchecked.defaultof<GlyphProps>), 1)
            | false -> struct (RunEdit.Finish, 1)

/// <summary>Measures the bytes one call allocates, after a warm-up call.</summary>
/// <param name="action">The call to measure.</param>
/// <returns>The bytes allocated by the measured call.</returns>
let private measureAlloc (action: unit -> unit) : int64 =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    action ()
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>Builds a run of <paramref name="count"/> glyphs with positions, for the per-edit allocation tests.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The run.</returns>
let private positionRun (count: int) : GlyphRun =
    let infos = Sealed.init count (fun i -> { Codepoint = uint32 (i % 128); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    let positions = Sealed.init count (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with Infos = infos; Props = props; Positions = positions; HavePositions = true }

/// <summary>A single-substitution lookup accelerator over a coverage and delta, <c>SingleSubstFormat1</c>.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="delta">The delta.</param>
/// <returns>The lookup accelerator.</returns>
let private singleSubstLookup (coverage: Coverage) (delta: uint32) : LookupAccel<SubstSubtable> =
    let subtable = SingleDelta (coverage, delta, 0xFFFFu)
    let digest = Coverage.digest coverage
    let accel = { Subtable = subtable; SubtableDigest = digest }
    { LookupType = 1
      Props = 0u
      IsReverse = false
      Subtables = ImmutableArray.Create accel
      LookupDigest = digest
      SecondDigest = Digest.full }

/// <summary>A synthetic GSUB table of one single-substitution lookup over a coverage and delta.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="delta">The delta.</param>
/// <returns>The table.</returns>
let private singleSubstTable (coverage: Coverage) (delta: uint32) : LayoutTable<SubstSubtable> =
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy (singleSubstLookup coverage delta))
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>A masked run of <paramref name="count"/> glyphs whose every glyph matches a range coverage.</summary>
/// <param name="count">The glyph count.</param>
/// <returns>The run.</returns>
let private maskedRun (count: int) : GlyphRun =
    let infos = Sealed.init count (fun index -> { Codepoint = uint32 (index % 128); Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>The PERF5 run-pass machine tests.</summary>
[<Tests>]
let tests =
    testList "PERF5" [
        test "RunEdit is 48 bytes" {
            Expect.equal (Unsafe.SizeOf<RunEdit>()) 48 "RunEdit must fit in 48 bytes"
        }
        testProperty "every constructor's edit matches the old machine" (
            Prop.forAll (Arb.fromGen genCase) (fun struct (run, edits) ->
                let m =
                    oldMachineOf
                        (run.Infos.AsSpan().ToArray())
                        (run.Props.AsSpan().ToArray())
                        (run.Positions.AsSpan().ToArray())
                        (byte run.Serial)
                        run.RandomState
                for edit in edits do
                    applyEdit m edit |> ignore
                applyEdit m OldEdit.Sync |> ignore
                let shaped = GlyphRun.runPass (ReplayPass(edits |> List.map toNew |> Array.ofList)) 0 run
                sameResult shaped m))
        test "a pass without edits allocates nothing" {
            let run =
                let infos = Sealed.init 1000 (fun i -> { Codepoint = uint32 (i % 128); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
                let props = Sealed.init 1000 (fun _ -> Unchecked.defaultof<GlyphProps>)
                { GlyphRun.empty with Infos = infos; Props = props }
            let measure count =
                let before = GC.GetAllocatedBytesForCurrentThread()
                let mutable index = 0
                while index < count do
                    GlyphRun.runPass (NoEditPass()) () run |> ignore
                    index <- index + 1
                GC.GetAllocatedBytesForCurrentThread() - before
            let mutable warm = 0
            while warm < 1000 do
                GlyphRun.runPass (NoEditPass()) () run |> ignore
                warm <- warm + 1
            Expect.equal (measure 1000) 0L "a no-edit pass allocates nothing"
        }
        test "PERF5 setting positions allocates nothing per edit" {
            let run = positionRun 10_000
            let one = measureAlloc (fun () -> GlyphRun.runPass (SetOnePositionPass()) 0 run |> ignore)
            let all = measureAlloc (fun () -> GlyphRun.runPass (SetPositionPass()) 0 run |> ignore)
            Expect.isLessThanOrEqual (abs (all - one)) 32_768L "9 999 extra SetPosition edits add no allocation beyond the one-edit pass (a box would add ~400 000 B)"
        }
        test "PERF5 outputting infos allocates nothing per edit" {
            let run = positionRun 10_000
            let one = measureAlloc (fun () -> GlyphRun.runPass (OutputOneInfoPass()) 0 run |> ignore)
            let all = measureAlloc (fun () -> GlyphRun.runPass (OutputInfoPass()) 0 run |> ignore)
            Expect.isLessThanOrEqual (abs (all - one)) 32_768L "9 999 extra OutputInfo edits add no allocation beyond the one-edit pass (a box would add ~400 000 B)"
        }
        test "PERF5 replacing every glyph allocates one working copy plus one output copy" {
            let run = maskedRun 1000
            let table = singleSubstTable (CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })) 1u
            let lookups = Sealed.init 1 (fun index ->
                { LookupIndex = index; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false; Mask = 1u; FeatureTag = Tag.make 'c' 'c' 'm' 'p' })
            let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
            let font = Font.create Face.empty
            let bytes = measureAlloc (fun () -> Engine.applyTable false (Engine.GsubApplier()) table lookups stages font run |> ignore)
            Expect.isLessThanOrEqual bytes 100_000L $"replacing 1 000 glyphs allocates {bytes} B (one materialised working copy plus one output copy, ~82 B/glyph)"
        }
    ]
