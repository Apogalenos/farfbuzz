namespace Farfbuzz

open System

/// <summary>The cluster-level glyph-flag propagation of <c>hb_propagate_flags</c>.</summary>
[<RequireQualifiedAccess>]
module internal Flags =
    /// <summary>Finds the end of an equal-cluster run, <c>foreach_cluster</c>'s boundary scan.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="cluster">The cluster of the run.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The first index whose cluster differs, or the length.</returns>
    [<TailCall>]
    let rec private clusterEnd (infos: ReadOnlySpan<GlyphInfo>) cluster index =
        match index >= infos.Length || infos[index].Cluster <> cluster with
        | true -> index
        | false -> clusterEnd infos cluster (index + 1)

    /// <summary>Ors the masks of a cluster range, <c>hb_propagate_flags</c>'s inner accumulation.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <param name="finish">The range end.</param>
    /// <param name="mask">The mask so far.</param>
    /// <returns>The accumulated mask.</returns>
    [<TailCall>]
    let rec private orMasks (infos: ReadOnlySpan<GlyphInfo>) index finish mask =
        match index >= finish with
        | true -> mask
        | false -> orMasks infos (index + 1) finish (mask ||| infos[index].Mask)

    /// <summary>The per-cluster mask mapper of <c>hb_propagate_flags</c>.</summary>
    [<Struct>]
    type private PropagateMapper =
        val AndMask: uint32
        val ProduceTatweel: bool
        new (andMask, produceTatweel) = { AndMask = andMask; ProduceTatweel = produceTatweel }

        interface IGroupMapper<GlyphInfo> with
            member this.GroupEnd (items, start) = clusterEnd items items[start].Cluster (start + 1)
            member this.Map (items, start, finish, index) =
                match this.ProduceTatweel with
                | true ->
                    let mask = orMasks items start finish 0u
                    let mask =
                        match mask &&& uint32 GlyphFlags.UnsafeToBreak <> 0u with
                        | true -> mask &&& ~~~(uint32 GlyphFlags.SafeToInsertTatweel)
                        | false -> mask
                    let mask =
                        match mask &&& uint32 GlyphFlags.SafeToInsertTatweel <> 0u with
                        | true -> mask ||| uint32 GlyphFlags.UnsafeToBreak ||| uint32 GlyphFlags.UnsafeToConcat
                        | false -> mask
                    { items[index] with Mask = mask &&& this.AndMask }
                | false ->
                    match finish - start with
                    | 1 -> { items[index] with Mask = items[index].Mask &&& this.AndMask }
                    | _ -> { items[index] with Mask = orMasks items start finish 0u &&& this.AndMask }

    /// <summary>The mask the propagated glyph flags are cut down to: the defined flags, without unsafe-to-concat unless
    /// the buffer produces it, <c>hb_propagate_flags</c>'s <c>and_mask</c>.</summary>
    /// <param name="flags">The buffer flags.</param>
    /// <returns>The mask.</returns>
    let andMaskOf flags =
        match flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default with
        | true -> uint32 GlyphFlags.Defined &&& ~~~(uint32 GlyphFlags.UnsafeToConcat)
        | false -> uint32 GlyphFlags.Defined

    /// <summary>Propagates cluster-level glyph flags, <c>hb_propagate_flags</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the flags propagated.</returns>
    let propagate run =
        let andMask = andMaskOf run.Flags
        let produceTatweel = run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel <> BufferFlags.Default
        let infos = Sealed.mapGroups (PropagateMapper(andMask, produceTatweel)) (run.Infos.AsSpan())
        { run with Infos = infos }

/// <summary>The ot shaping pipeline, <c>hb_ot_shape_internal</c>.</summary>
[<RequireQualifiedAccess>]
module internal OtShaper =
    /// <summary>Runs the stages that follow positioning, <c>hb_ot_shape_internal</c>'s tail: the post-substitution
    /// hook, the flag propagation and the restore of the original direction.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="targetDirection">The original direction of the run.</param>
    /// <param name="positioned">The positioned run.</param>
    /// <returns>The shaped run.</returns>
    let afterPosition plan font targetDirection positioned =
        let post = OtSubstitute.substitutePost plan font positioned
        GlyphRun.withDirection targetDirection (Flags.propagate post)

    /// <summary>Runs the positioning stages and the tail on a substituted run, <c>hb_ot_shape_internal</c>'s second
    /// half.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="targetDirection">The original direction of the run.</param>
    /// <param name="substituted">The run after the substitution stages.</param>
    /// <returns>The shaped run.</returns>
    let fromSubstituted plan font targetDirection substituted =
        match OtPosition.position plan font substituted with
        | Error error -> Error error
        | Ok positioned -> Ok (afterPosition plan font targetDirection positioned)

    /// <summary>Runs the pipeline up to and including the normalizer, <c>hb_ot_shape_internal</c>'s first half and
    /// <c>hb_ot_substitute_pre</c>'s rotation and normalization.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The normalized run.</returns>
    let normalised plan font run =
        let circled =
            match plan.Unicode with
            | ValueNone -> OtProps.insertDottedCircle font (OtProps.setUnicodePropsFused plan.Map.GlobalMask run)
            | ValueSome funcs ->
                OtProps.insertDottedCircleWith funcs font (OtProps.setUnicodePropsWith funcs plan.Map.GlobalMask run)
        let clustered = OtProps.formClusters circled
        let native = OtProps.ensureNativeDirection clustered
        let prepared = ShaperHooks.preprocess plan font native
        Normalize.run plan font (OtSubstitute.rotateChars plan font plan.Props.Direction prepared)

    /// <summary>Runs the whole ot pipeline, <c>hb_ot_shape_internal</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let shape plan font run =
        let targetDirection = run.SegmentProperties.Direction
        match OtSubstitute.afterNormalize plan font (normalised plan font run) with
        | Error error -> Error error
        | Ok substituted -> fromSubstituted plan font targetDirection substituted
