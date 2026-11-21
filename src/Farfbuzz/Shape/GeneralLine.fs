namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Aat
open Farfbuzz.OpenType

/// <summary>The general line on one working set: a line that keeps its marks is shaped through the stages of
/// <see cref="OtShaper.shape"/> on the arrays its first stage makes, every later stage that keeps the length of the line
/// writing into them in place, and the run is published once at the end. The result is that of <see cref="Execute.runGeneral"/>
/// in every field; a line the stages cannot prove goes back to the general route from the run it came in.</summary>
[<RequireQualifiedAccess>]
module internal GeneralLine =
    /// <summary>The first code point that can be a mark, <c>0x0300</c>: a line without a code point from here on after its first has no
    /// mark and takes the route of the lines without marks.</summary>
    [<Literal>]
    let private FirstMark = 0x0300u

    /// <summary>The grapheme grouping: a glyph and its following continuations, <c>_hb_grapheme_group_func</c>.</summary>
    [<Struct>]
    type private GraphemeGrouping =
        interface IGlyphGrouping with
            member _.Group (_, current) = UnicodeProps.isContinuation current

    /// <summary>Whether a font maps a code point, <c>hb_font_has_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the font has a nominal glyph for it.</returns>
    let private hasGlyph (font: Font) (codepoint: uint32) =
        match Font.nominalGlyph font codepoint with
        | ValueSome _ -> true
        | ValueNone -> false

    /// <summary>The mirroring and rotation of one glyph's code point for the direction of the plan, <c>hb_ot_rotate_chars</c>.</summary>
    [<Struct>]
    type private RotateMapper =
        val Font: Font
        val RtlmMask: uint32
        val Backward: bool
        val Vertical: bool

        /// <summary>Creates the mapper.</summary>
        /// <param name="font">The font.</param>
        /// <param name="rtlmMask">The mask of the rtlm feature.</param>
        /// <param name="backward">Whether the direction is backward.</param>
        /// <param name="vertical">Whether the code points are rotated.</param>
        new (font, rtlmMask, backward, vertical) = { Font = font; RtlmMask = rtlmMask; Backward = backward; Vertical = vertical }

        interface IGroupMapper<GlyphInfo> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                let info = items[index]
                let mirrored =
                    match this.Backward with
                    | false -> info
                    | true ->
                        let mirror = Unicode.mirroring info.Codepoint
                        match mirror <> info.Codepoint && hasGlyph this.Font mirror with
                        | true -> { info with Codepoint = mirror }
                        | false -> { info with Mask = info.Mask ||| this.RtlmMask }
                match this.Vertical with
                | false -> mirrored
                | true ->
                    let verticalChar = Unicode.verticalCharFor mirrored.Codepoint
                    match verticalChar <> mirrored.Codepoint && hasGlyph this.Font verticalChar with
                    | true -> { mirrored with Codepoint = verticalChar }
                    | false -> mirrored

    /// <summary>The class of one glyph from GDEF, with its ligature properties cleared, <c>_hb_ot_layout_set_glyph_props</c>.</summary>
    [<Struct>]
    type private GlyphClassMapper =
        val Font: Font
        val Infos: ImmutableArray<GlyphInfo>

        /// <summary>Creates the mapper.</summary>
        /// <param name="font">The font.</param>
        /// <param name="infos">The glyphs, whose code points are glyph indices.</param>
        new (font, infos) = { Font = font; Infos = infos }

        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                { items[index] with GlyphClass = OtLayout.glyphProps this.Font this.Infos[index].Codepoint; LigProps = 0uy }

    /// <summary>The base or mark class of one glyph without GDEF, <c>hb_synthesize_glyph_classes</c>.</summary>
    [<Struct>]
    type private SynthesizeMapper =
        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member _.Map (items, _, _, index) =
                let props = items[index]
                match not (UnicodeProps.isNonSpacingMark props) || UnicodeProps.isDefaultIgnorable props with
                | true -> { props with GlyphClass = GlyphPropsBits.Base }
                | false -> { props with GlyphClass = GlyphPropsBits.Mark }

    /// <summary>Whether a line holds a code point that can be a mark after its first.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when a code point from <c>0x0300</c> on follows the first.</returns>
    [<TailCall>]
    let rec private hasLateCodepoint (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> false
        | false ->
            match infos[index].Codepoint >= FirstMark with
            | true -> true
            | false -> hasLateCodepoint infos (index + 1)

    /// <summary>The fields of a run the in-place stages read, with the positions of the run or others.</summary>
    /// <param name="run">The run.</param>
    /// <param name="positions">The positions the stage reads.</param>
    /// <param name="havePositions">Whether the positions are real.</param>
    /// <returns>The input.</returns>
    let private inputWith (run: GlyphRun) positions havePositions =
        { Infos = run.Infos
          Props = run.Props
          Positions = positions
          HavePositions = havePositions
          MaxLen = run.MaxLen
          MaxOps = run.MaxOps
          Successful = run.Successful
          ClusterLevel = run.ClusterLevel
          Flags = run.Flags
          Serial = byte run.Serial
          RandomState = run.RandomState
          ScratchFlags = run.ScratchFlags }

    /// <summary>The fields of a run the in-place stages read.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The input.</returns>
    let private inputOf (run: GlyphRun) =
        inputWith run run.Positions run.HavePositions

    /// <summary>Merges the clusters of every grapheme in place, <c>hb_form_clusters</c>, on a run whose arrays this call made.</summary>
    /// <param name="run">The run, with owned arrays.</param>
    /// <returns>The run with grapheme clusters merged.</returns>
    let internal formClusters (run: GlyphRun) =
        match run.ScratchFlags &&& ScratchFlags.HasContinuations = ScratchFlags.Default with
        | true -> run
        | false ->
            let result = Sealed.mergeGroupsOwned (GraphemeGrouping()) (inputOf run)
            match result.Unchanged with
            | true -> run
            | false -> GlyphRun.adopt run result

    /// <summary>Mirrors and rotates the code points for the target direction in the glyph array of a line whose arrays this call made,
    /// <c>hb_ot_rotate_chars</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs, with owned arrays.</param>
    let private rotateInPlace (plan: OtShapePlan) (font: Font) (infos: ImmutableArray<GlyphInfo>) =
        let direction = plan.Props.Direction
        let backward = Direction.isBackward direction
        let vertical =
            match direction with
            | Direction.Ttb | Direction.Btt -> not plan.HasVert
            | Direction.Invalid | Direction.Ltr | Direction.Rtl -> false
        match backward || vertical with
        | false -> ()
        | true -> Sealed.mapOwned (RotateMapper(font, plan.RtlmMask, backward, vertical)) infos

    /// <summary>Mirrors and rotates the code points for the target direction in place, <c>hb_ot_rotate_chars</c>, on a run whose arrays
    /// this call made.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with owned arrays.</param>
    /// <returns>The run with mirrored or rotated code points.</returns>
    let internal rotateChars (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        rotateInPlace plan font run.Infos
        run

    /// <summary>Mirrors and rotates the code points of a working set as <see cref="rotateChars"/> does a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with owned arrays.</param>
    /// <returns>The set with mirrored or rotated code points.</returns>
    let internal rotateCharsSet (plan: OtShapePlan) (font: Font) (set: WorkingSet) =
        rotateInPlace plan font set.Infos
        set

    /// <summary>Merges the clusters of every grapheme of a working set in place, <c>hb_form_clusters</c>, as <see cref="formClusters"/> does a run.</summary>
    /// <param name="set">The working set, with owned arrays.</param>
    /// <returns>The set with grapheme clusters merged.</returns>
    let internal formClustersSet (set: WorkingSet) =
        match set.ScratchFlags &&& ScratchFlags.HasContinuations = ScratchFlags.Default with
        | true -> set
        | false -> GlyphRun.applyResult set (Sealed.mergeGroupsOwned (GraphemeGrouping()) (GlyphRun.inputOfSet set))

    /// <summary>Inserts the dotted circle before a leading mark, <c>hb_insert_dotted_circle</c>, into a working set: a set the conditions do
    /// not hold for is returned as it is, and the rest takes the pass on a run.</summary>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set.</param>
    /// <returns>The set, with a dotted circle prepended when the conditions hold.</returns>
    let internal insertDottedCircleSet (font: Font) (set: WorkingSet) =
        match set.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default
              && set.Flags &&& BufferFlags.Bot <> BufferFlags.Default
              && set.ContextBefore.IsEmpty
              && set.Props.Length > 0
              && UnicodeProps.isUnicodeMark set.Props[0] with
        | false -> set
        | true -> GlyphRun.setOf (OtProps.insertDottedCircle font (GlyphRun.runOf set))

    /// <summary>Reverses graphemes and flips the direction when it differs from the native one, <c>hb_ensure_native_direction</c>, on a
    /// working set: a set whose direction is the native one of its script is returned as it is, and the rest takes the function on a run.</summary>
    /// <param name="set">The working set.</param>
    /// <returns>The set in its native direction.</returns>
    let internal ensureNativeDirectionSet (set: WorkingSet) =
        match set.SegmentProperties.Direction = Script.horizontalDirection set.SegmentProperties.Script with
        | true -> set
        | false -> GlyphRun.setOf (OtProps.ensureNativeDirection (GlyphRun.runOf set))

    /// <summary>Runs the stages of the front half that precede the normaliser on one working set, <c>hb_ot_shape_internal</c>'s first
    /// stages: the Unicode properties make the arrays, and the clusters and the rotation write into them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, as it came in.</param>
    /// <returns>The run before the normaliser, whose arrays this call made.</returns>
    let internal beforeNormaliser (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let propped = OtProps.setUnicodePropsEntered plan.Map.GlobalMask run
        let circled = OtProps.insertDottedCircle font propped
        let clustered = formClusters circled
        let native = OtProps.ensureNativeDirection clustered
        let prepared = ShaperHooks.preprocessScanned plan font native
        rotateChars plan font prepared

    /// <summary>Runs the stages of the front half that follow the Unicode properties and precede the normaliser on a working set: the dotted
    /// circle, the clusters, the native direction, the shaper's preprocessing and the rotation, every stage writing into the arrays the
    /// properties made.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="propped">The working set with its Unicode properties, whose arrays the properties stage made.</param>
    /// <returns>The working set before the normaliser.</returns>
    let internal afterPropertiesSet (plan: OtShapePlan) (font: Font) (propped: WorkingSet) =
        propped
        |> insertDottedCircleSet font
        |> formClustersSet
        |> ensureNativeDirectionSet
        |> ShaperHooks.preprocessSet plan font
        |> rotateCharsSet plan font

    /// <summary>Runs the stages of the front half that precede the normaliser on one working set, as <see cref="beforeNormaliser"/> does,
    /// without a run record: the Unicode properties make the arrays, and every later stage writes into them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, as it came in.</param>
    /// <returns>The working set before the normaliser, whose arrays this call made.</returns>
    let internal beforeNormaliserSet (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        afterPropertiesSet plan font (OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask run)

    /// <summary>Runs the stages between the normaliser and the substitution on the working set, <c>hb_ot_substitute_pre</c>'s second half
    /// up to the lookups: the glyphs and the glyph classes are written in place.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="normalized">The normalized run, whose arrays this call made.</param>
    /// <returns>The run with its glyphs and classes, ready for the lookups.</returns>
    let internal glyphsOf (plan: OtShapePlan) (font: Font) (normalized: GlyphRun) =
        let masked = OtSubstitute.setupMasksOwned plan normalized
        let recategorized =
            match plan.FallbackMarkPositioning with
            | true -> OtLayout.recategorizeMarks masked
            | false -> masked
        Sealed.mapGlyphsOwned recategorized.Infos recategorized.Props
        match Gdef.hasGlyphClasses font.Ot.Gdef.Value with
        | false -> ()
        | true -> Sealed.mapOwned (GlyphClassMapper(font, recategorized.Infos)) recategorized.Props
        match plan.FallbackGlyphClasses with
        | false -> ()
        | true -> Sealed.mapOwned (SynthesizeMapper()) recategorized.Props
        { recategorized with ContentType = GlyphContent }

    /// <summary>Runs the stages between the normaliser and the substitution on a working set as <see cref="glyphsOf"/> does: the masks, the
    /// glyphs and the glyph classes are written in place.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="normalized">The normalized set, whose arrays this call made.</param>
    /// <returns>The set with its glyphs and classes, ready for the lookups.</returns>
    let internal glyphsOfSet (plan: OtShapePlan) (font: Font) (normalized: WorkingSet) =
        let masked = OtSubstitute.setupMasksOwnedSet plan normalized
        let recategorized =
            match plan.FallbackMarkPositioning with
            | true -> FallbackMark.recategorizeMarksSet masked
            | false -> masked
        Sealed.mapGlyphsOwned recategorized.Infos recategorized.Props
        match Gdef.hasGlyphClasses font.Ot.Gdef.Value with
        | false -> ()
        | true -> Sealed.mapOwned (GlyphClassMapper(font, recategorized.Infos)) recategorized.Props
        match plan.FallbackGlyphClasses with
        | false -> ()
        | true -> Sealed.mapOwned (SynthesizeMapper()) recategorized.Props
        { recategorized with ContentType = GlyphContent }

    /// <summary>Applies the substitution lookups of the plan to a working set with its glyphs and classes: through the glyph sets the plan holds,
    /// so that a lookup is walked only over the glyphs its subtables cover, unless the map has a pause that runs a function, whose function
    /// is written in place on the set.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="synthesized">The set with its glyphs and classes.</param>
    /// <returns>The substituted set, and whether it is a line rebuilt from learned syllables with an upper bound of its operations, to be shaped
    /// again from its input should it run out of them.</returns>
    let internal lookupsSet (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (synthesized: WorkingSet) =
        let struct (substituted, rebuilt) =
            match otPlan.ApplyMorx, otPlan.Map.HasPauses with
            | true, _ -> struct (GlyphRun.setOf (AatLayout.substitute otPlan font (otPlan.UserFeatures.AsSpan()) (GlyphRun.runOf synthesized)), false)
            | false, true ->
                match plan.Syllables with
                | ValueSome program -> SyllableReuse.substitute program plan otPlan font synthesized
                | ValueNone -> struct (OtLayout.substituteWindowedSet otPlan font plan.GsubLookupCovered plan.GsubWindows synthesized, false)
            | false, false -> struct (Engine.substituteApplicableOwnedSet otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups synthesized, false)
        match otPlan.ApplyMorx && otPlan.ApplyGpos with
        | true -> struct (GlyphRun.setOf (AatLayout.removeDeletedGlyphs (GlyphRun.runOf substituted)), rebuilt)
        | false -> struct (substituted, rebuilt)

    /// <summary>Applies the substitution lookups of the plan to a run with its glyphs and classes, as <see cref="lookupsSet"/> does a working set.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="synthesized">The run with its glyphs and classes, whose arrays this call made.</param>
    /// <returns>The substituted run; a line rebuilt from learned syllables is not checked against running out of operations, as the callers
    /// measure this stage alone.</returns>
    let internal lookups (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (synthesized: GlyphRun) =
        let struct (substituted, _) = lookupsSet plan otPlan font (GlyphRun.setOf synthesized)
        GlyphRun.runOf substituted

    /// <summary>Resets the limits and the serial of a shaped run, <c>Execute.run</c>'s tail.</summary>
    /// <param name="run">The shaped run.</param>
    /// <returns>The run with the default limits.</returns>
    let private reset (run: GlyphRun) =
        { run with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }

    /// <summary>Whether the plan zeroes the advances of marks at a stage.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="stage">The stage, early before the GPOS pass or late after it.</param>
    /// <returns>True when the marks are zeroed at the stage.</returns>
    let private zeroesAt (plan: OtShapePlan) stage =
        plan.ZeroMarks && plan.Shaper.ZeroWidthMarks = stage

    /// <summary>Finishes the offsets of a positioned working set, <c>GPOS::position_finish_offsets</c>: in place, or through the general
    /// function on a run for a slanted font.</summary>
    /// <param name="font">The font.</param>
    /// <param name="set">The set, with the positions after the GPOS pass.</param>
    /// <returns>The set with the offsets finished.</returns>
    let private finishOffsetsSet (font: Font) (set: WorkingSet) =
        let horizontal = Direction.isHorizontal set.SegmentProperties.Direction
        match font.SlantXY <> 0f && horizontal with
        | true -> GlyphRun.setOf (GposApply.finishOffsets font (GlyphRun.runOf set))
        | false ->
            match set.ScratchFlags &&& ScratchFlags.HasGposAttachment = ScratchFlags.Default with
            | true -> set
            | false ->
                Sealed.propagateAttachmentsOwned set.Positions (not (Direction.isBackward set.SegmentProperties.Direction)) horizontal
                set

    /// <summary>Finishes the offsets of a positioned working set: the default-ignorable widths and the attachment offsets,
    /// <c>hb_ot_shape_internal</c>'s tail up to the fallback marks.</summary>
    /// <param name="font">The font.</param>
    /// <param name="positioned">The set after the GPOS pass and the late zeroing of marks, whose arrays this call made.</param>
    /// <returns>The set with the offsets finished.</returns>
    let private offsetsOf (font: Font) (positioned: WorkingSet) =
        let horizontal = Direction.isHorizontal positioned.SegmentProperties.Direction
        match positioned.ScratchFlags &&& ScratchFlags.HasDefaultIgnorables <> ScratchFlags.Default
              && positioned.Flags &&& BufferFlags.PreserveDefaultIgnorables = BufferFlags.Default
              && positioned.Flags &&& BufferFlags.RemoveDefaultIgnorables = BufferFlags.Default with
        | true -> Sealed.zeroIgnorablesOwned positioned.Props UnicodeProps.Ignorable horizontal positioned.Positions
        | false -> ()
        finishOffsetsSet font positioned

    /// <summary>Positions the marks that have no anchor of the font, <c>_hb_ot_shape_fallback_mark_position</c>, in a working set whose plan asks for
    /// it: a set whose clusters need no mark positioning is returned as it is, and the rest takes the function on a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjust">Whether the offsets are adjusted as the advances are zeroed.</param>
    /// <param name="set">The set with its offsets finished.</param>
    /// <returns>The set with the marks positioned, or the error of the function.</returns>
    let private fallbackMarksSet (plan: OtShapePlan) (font: Font) (adjust: bool) (set: WorkingSet) =
        match plan.FallbackMarkPositioning with
        | false -> Ok set
        | true ->
            match OtLayout.fallbackMarkPosition plan font adjust (GlyphRun.runOf set) with
            | Error error -> Error error
            | Ok positioned -> Ok (GlyphRun.setOf positioned)

    /// <summary>Closes a positioned working set: the reversal of a backward run, the post hook and the flag propagation, the end of
    /// <c>hb_ot_shape_internal</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="offset">The set with its offsets finished and its fallback marks positioned.</param>
    /// <returns>The shaped set and the operations it had left before its limits were reset.</returns>
    let private closedSetLeft (plan: OtShapePlan) (font: Font) (target: Direction) (offset: WorkingSet) =
        match Direction.isBackward offset.SegmentProperties.Direction with
        | true -> Sealed.reverseOwned offset.Infos offset.Props offset.Positions
        | false -> ()
        let post =
            match offset.ScratchFlags &&& (ScratchFlags.HasVariationSelectorFallback ||| ScratchFlags.HasDefaultIgnorables ||| ScratchFlags.Shaper0) = ScratchFlags.Default with
            | true -> offset
            | false -> GlyphRun.setOf (OtSubstitute.substitutePost plan font (GlyphRun.runOf offset))
        Sealed.propagateFlagsOwned post.Infos (Flags.andMaskOf post.Flags) (post.Flags &&& BufferFlags.ProduceSafeToInsertTatweel <> BufferFlags.Default)
        struct ({ post with SegmentProperties = { post.SegmentProperties with Direction = target }; MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }, post.MaxOps)

    /// <summary>Closes a positioned working set, <see cref="closedSetLeft"/> without the operations left.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="offset">The set with its offsets finished and its fallback marks positioned.</param>
    /// <returns>The shaped set.</returns>
    let private closedSet (plan: OtShapePlan) (font: Font) (target: Direction) (offset: WorkingSet) =
        let struct (closed, _) = closedSetLeft plan font target offset
        closed

    /// <summary>Finishes a positioned working set into the one run it publishes: the default-ignorable widths, the attachment offsets,
    /// the reversal of a backward run, the post hook and the flag propagation, <c>hb_ot_shape_internal</c>'s tail, for a plan that does not
    /// position fallback marks.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="positioned">The set after the GPOS pass and the late zeroing of marks, whose arrays this call made.</param>
    /// <returns>The shaped set.</returns>
    let internal finishedSet (plan: OtShapePlan) (font: Font) (target: Direction) (positioned: WorkingSet) =
        closedSet plan font target (offsetsOf font positioned)

    /// <summary>Finishes a positioned run on the working set, <see cref="finishedSet"/> of the set the run holds.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="positioned">The run after the GPOS pass and the late zeroing of marks.</param>
    /// <returns>The shaped run.</returns>
    let internal finished (plan: OtShapePlan) (font: Font) (target: Direction) (positioned: GlyphRun) =
        GlyphRun.runOf (finishedSet plan font target (GlyphRun.setOf positioned))

    /// <summary>Runs the GPOS pass and the zeroing of marks on the working set, in place, from the default advances.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="substituted">The run after the substitution, whose arrays this call made.</param>
    /// <param name="advances">The default positions of its glyphs.</param>
    /// <returns>The positioned run, or none when the GPOS pass failed and the general route must run from the run it came in.</returns>
    let internal gposPass (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (substituted: GlyphRun) (advances: ImmutableArray<GlyphPosition>) =
        match zeroesAt otPlan ZeroWidthMarksByGdefEarly with
        | true -> Sealed.zeroMarkAdvancesOwned substituted.Props advances
        | false -> ()
        let struct (maxOps, successful, scratch) = Engine.positionPlannedOwned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs substituted.SegmentProperties.Direction (inputWith substituted advances true)
        match successful with
        | false -> ValueNone
        | true ->
            match zeroesAt otPlan ZeroWidthMarksByGdefLate with
            | true -> Sealed.zeroMarkAdvancesOwned substituted.Props advances
            | false -> ()
            ValueSome { substituted with Positions = advances; HavePositions = true; MaxOps = maxOps; ScratchFlags = scratch }

    /// <summary>The default positions of the glyphs of a line, from the horizontal metrics of the font.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The positions, or none when a glyph is not one the simple advances serve.</returns>
    let internal advancesOf (otPlan: OtShapePlan) (font: Font) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) =
        let metrics = font.Ot.Hmtx.Value.Table.Span
        Sealed.simpleAdvances (SimpleRun.SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (infos.AsSpan()) (props.AsSpan()) metrics 0us 0us

    /// <summary>The default positions of the glyphs of a substituted run, from the horizontal metrics of the font.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="substituted">The run after the substitution.</param>
    /// <returns>The positions, or none when a glyph is not one the simple advances serve.</returns>
    let internal defaultAdvances (otPlan: OtShapePlan) (font: Font) (substituted: GlyphRun) =
        advancesOf otPlan font substituted.Infos substituted.Props

    /// <summary>Whether the marks are positioned by adjusting their offsets as their advances are zeroed, as <c>position</c> decides it:
    /// the plan asks for it and the direction is a forward one.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="set">The set whose direction decides.</param>
    /// <returns>True when the offsets are adjusted.</returns>
    let private adjustsOffsets (otPlan: OtShapePlan) (set: WorkingSet) =
        otPlan.AdjustMarkPositioningWhenZeroing && not (Direction.isBackward set.SegmentProperties.Direction)

    /// <summary>Whether a substituted line takes the lean positioning, <c>SimpleRun.isMarkedLine</c> on a working set, and a line of a plan
    /// without GPOS as well, one that adjusts the offsets of marks as their advances are zeroed and one whose fallback marks are positioned after
    /// the offsets are finished: the default advances, the zeroing of marks and the finishing are all the positioning such a plan has.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="set">The set after the substitution.</param>
    /// <returns>True when the lean positioning applies.</returns>
    let private isMarkedSet (otPlan: OtShapePlan) (set: WorkingSet) =
        set.Successful
        && Direction.isHorizontal set.SegmentProperties.Direction
        && not otPlan.ApplyKern
        && not otPlan.ApplyTrak
        && not otPlan.ApplyKerx
        && set.ScratchFlags &&& ScratchFlags.HasSpaceFallback = ScratchFlags.Default

    /// <summary>Runs the GPOS pass and the zeroing of marks on a working set in place, from the default advances, as <see cref="gposPass"/> does
    /// for a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The set after the substitution, whose arrays this call made.</param>
    /// <param name="advances">The default positions of its glyphs, written in place.</param>
    /// <returns>The operation budget and the scratch flags the pass left, or none when the GPOS pass failed.</returns>
    let internal gposPartsSet (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (set: WorkingSet) (advances: ImmutableArray<GlyphPosition>) =
        let adjust = adjustsOffsets otPlan set
        match zeroesAt otPlan ZeroWidthMarksByGdefEarly with
        | true -> Sealed.zeroMarkWidthsOwned adjust set.Props advances
        | false -> ()
        let input = { GlyphRun.inputOfSet set with Positions = advances; HavePositions = true }
        let struct (maxOps, successful, scratch) =
            match otPlan.ApplyGpos with
            | true -> Engine.positionPlannedOwned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs set.SegmentProperties.Direction input
            | false -> struct (set.MaxOps, true, set.ScratchFlags)
        match successful with
        | false -> ValueNone
        | true ->
            match zeroesAt otPlan ZeroWidthMarksByGdefLate with
            | true -> Sealed.zeroMarkWidthsOwned adjust set.Props advances
            | false -> ()
            ValueSome (struct (maxOps, scratch))

    /// <summary>Finishes a working set whose GPOS pass has run: the offsets, the fallback marks of a plan that positions them, the reversal, the
    /// post hook and the flags.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="set">The set after the substitution.</param>
    /// <param name="advances">The positions after the GPOS pass.</param>
    /// <param name="parts">The operation budget and the scratch flags the pass left.</param>
    /// <returns>The shaped set and the operations it had left before its limits were reset, or the error of the fallback marks.</returns>
    let private closedLean (otPlan: OtShapePlan) (font: Font) (target: Direction) (set: WorkingSet) (advances: ImmutableArray<GlyphPosition>) (parts: struct (int * ScratchFlags)) =
        let struct (maxOps, scratch) = parts
        let offsets = offsetsOf font { set with Positions = advances; HavePositions = true; MaxOps = maxOps; ScratchFlags = scratch }
        match fallbackMarksSet otPlan font (adjustsOffsets otPlan set) offsets with
        | Error error -> Error error
        | Ok marked -> Ok (closedSetLeft otPlan font target marked)

    /// <summary>Positions a substituted working set that takes the lean positioning: the default advances are the one array the positions
    /// need, and the GPOS machine, the zeroing of marks, the offsets, the reversal and the flags write into it and into the glyphs.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="set">The set after the substitution, whose arrays this call made.</param>
    /// <returns>The shaped set and the operations it had left before its limits were reset, or none when the general route must run from the
    /// run it came in.</returns>
    let private positionedLean (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (set: WorkingSet) =
        match advancesOf otPlan font set.Infos set.Props with
        | ValueNone -> ValueNone
        | ValueSome advances ->
            match gposPartsSet plan otPlan font set advances with
            | ValueNone -> ValueNone
            | ValueSome parts -> ValueSome (closedLean otPlan font target set advances parts)

    /// <summary>Positions a substituted working set and publishes it as one run, as <see cref="SimpleRun.shapeGeneral"/> does on copies: a line
    /// that takes the lean positioning is written in place, and any other takes the stages of the reference.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="set">The set after the substitution, whose arrays this call made.</param>
    /// <returns>The shaped set and the operations it had left before its limits were reset, or none when the general route must run from the
    /// run it came in.</returns>
    let private positionedSetLeft (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (set: WorkingSet) =
        match isMarkedSet otPlan set with
        | true -> positionedLean plan otPlan font target set
        | false ->
            match OtShaper.fromSubstituted otPlan font target (GlyphRun.runOf set) with
            | Error error -> ValueSome (Error error)
            | Ok shaped -> ValueSome (Ok struct (GlyphRun.setOf (reset shaped), shaped.MaxOps))

    /// <summary>Positions a substituted working set and publishes it as one run, <see cref="positionedSetLeft"/> without the operations left.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="set">The set after the substitution, whose arrays this call made.</param>
    /// <returns>The shaped set, or none when the general route must run from the run it came in.</returns>
    let internal positionedSet (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (set: WorkingSet) =
        match positionedSetLeft plan otPlan font target set with
        | ValueNone -> ValueNone
        | ValueSome (Error error) -> ValueSome (Error error)
        | ValueSome (Ok struct (shaped, _)) -> ValueSome (Ok shaped)

    /// <summary>Positions a line rebuilt from learned syllables with an upper bound of its operations: when it ends with no operation left, the
    /// one case in which the lower budget it ran with could have changed its glyphs, or fails, the line is left to the general route, which shapes
    /// it again from the run it came in without the reuse.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="set">The rebuilt set, whose arrays this call made.</param>
    /// <returns>The shaped set, or none when the general route must run from the run it came in: when the line ran out of operations, the one case
    /// in which the upper bound of the rebuilt line's operations would change its glyphs, or failed.</returns>
    let private positionedChecked (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (set: WorkingSet) =
        match positionedSetLeft plan otPlan font target set with
        | ValueSome (Ok struct (shaped, left)) when left > 0 -> ValueSome (Ok shaped)
        | ValueNone | ValueSome (Ok _) | ValueSome (Error _) -> ValueNone

    /// <summary>Whether the kernel's tails can take a normalized line, <c>SimpleRun.handOff</c>'s necessary conditions that a working set answers
    /// without a run being made of it: a successful left-to-right line of the default shaper, in a script that is not right-to-left.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="normalized">The normalized set.</param>
    /// <returns>False when the hand-off would refuse the line.</returns>
    let private mayHandOff (otPlan: OtShapePlan) (normalized: WorkingSet) =
        normalized.Successful
        && normalized.SegmentProperties.Direction = Direction.Ltr
        && Script.horizontalDirection normalized.SegmentProperties.Script <> Direction.Rtl
        && (match otPlan.Shaper.Kind with
            | DefaultShaper -> true
            | DumberShaper | HebrewShaper | ThaiShaper | ArabicShaper | IndicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper | UseShaper | HangulShaper -> false)

    /// <summary>Whether the working-set route shapes the lines of a shaper: the shapers the lean normaliser reads, and Hangul, whose
    /// normalisation mode is none, so that the out-buffer normaliser the route falls back to charges the operations the reference does.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <returns>True when the shaper's lines may take the route.</returns>
    let private servesShaper (otPlan: OtShapePlan) =
        match otPlan.Shaper.Kind with
        | HangulShaper -> true
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | ArabicShaper | IndicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper | UseShaper ->
            ShaperHooks.leanNormalisation otPlan

    /// <summary>Whether the working-set route takes a run: it succeeded, its shaper is one the route serves, and some code point after the
    /// first can be a mark.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>True when the route may shape the run.</returns>
    let private takes (otPlan: OtShapePlan) (run: GlyphRun) =
        run.Successful && servesShaper otPlan && hasLateCodepoint (run.Infos.AsSpan()) 1

    /// <summary>Shapes a normalized working set through the rest of the stages of <see cref="OtShaper.shape"/>: the kernel's tails when they
    /// take the line, and the glyphs, the lookups, the positioning and the finishing otherwise.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="normalized">The normalized set, whose arrays this call made.</param>
    /// <returns>The shaped set, or none when the general route must run from the run it came in.</returns>
    let private shapeNormalized (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (normalized: WorkingSet) =
        let handed =
            match mayHandOff otPlan normalized && not (Sealed.anyUnicodeIn (normalized.Props.AsSpan()) ~~~UnicodeProps.GenCatMask) with
            | false -> ValueNone
            | true -> SimpleRun.handOff plan otPlan font target (GlyphRun.runOf normalized)
        match handed with
        | ValueSome (Ok shaped) -> ValueSome (Ok (GlyphRun.setOf shaped))
        | ValueSome (Error error) -> ValueSome (Error error)
        | ValueNone ->
            let struct (substituted, rebuilt) = lookupsSet plan otPlan font (glyphsOfSet otPlan font normalized)
            match rebuilt with
            | false -> positionedSet plan otPlan font target substituted
            | true -> positionedChecked plan otPlan font target substituted

    /// <summary>Shapes a line that keeps its marks on one working set, through the stages of <see cref="OtShaper.shape"/>, and leaves the
    /// shaped line in the set: the glyphs and positions of <see cref="Execute.runGeneral"/>, for a caller that copies them out of the set
    /// itself.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The shaped set, or none when the run is not one the stages shape on the working set.</returns>
    let internal tryShapeSet (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match takes otPlan run with
        | false -> ValueNone
        | true -> shapeNormalized plan otPlan font run.SegmentProperties.Direction (Normalize.runOwnedSet otPlan font (beforeNormaliserSet otPlan font run))

    /// <summary>Shapes a line that keeps its marks as <see cref="tryShapeSet"/> does, for a run the caller decoded in this call and shares
    /// with no one: the masks are written into the glyphs of the run, so that the run's glyphs are the shaped set's, and the run is not
    /// to be read or shaped again whatever the answer, a refusal included.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty, with glyphs of its own.</param>
    /// <returns>The shaped set, or none when the run is not one the stages shape on the working set.</returns>
    let internal tryShapeFreshSet (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match takes otPlan run with
        | false -> ValueNone
        | true -> shapeNormalized plan otPlan font run.SegmentProperties.Direction (Normalize.runOwnedSet otPlan font (afterPropertiesSet otPlan font (OtProps.setUnicodePropsFreshSet otPlan.Map.GlobalMask run)))

    /// <summary>Shapes a line that keeps its marks on one working set, through the stages of <see cref="OtShaper.shape"/>: the result of
    /// <see cref="Execute.runGeneral"/> in every field.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The shaped run, or none when the run is not one the stages shape on the working set.</returns>
    let internal tryShape (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match tryShapeSet plan otPlan font run with
        | ValueNone -> ValueNone
        | ValueSome (Error error) -> ValueSome (Error error)
        | ValueSome (Ok shaped) -> ValueSome (Ok (GlyphRun.runOf shaped))

    /// <summary>Shapes a line that keeps its marks as <see cref="tryShape"/> does, for a run the caller decoded in this call and shares with
    /// no one, as <see cref="tryShapeFreshSet"/> does a set.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty, with glyphs of its own.</param>
    /// <returns>The shaped run, or none when the run is not one the stages shape on the working set.</returns>
    let internal tryShapeFresh (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match tryShapeFreshSet plan otPlan font run with
        | ValueNone -> ValueNone
        | ValueSome (Error error) -> ValueSome (Error error)
        | ValueSome (Ok shaped) -> ValueSome (Ok (GlyphRun.runOf shaped))
