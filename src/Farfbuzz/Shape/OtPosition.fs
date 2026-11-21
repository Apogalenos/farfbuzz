namespace Farfbuzz

open System
open Farfbuzz.Aat

/// <summary>The positioning stages of the ot pipeline: default advances, fallback spaces, zero widths and the plan composition.</summary>
[<RequireQualifiedAccess>]
module internal OtPosition =
    /// <summary>The space code point, the source of the invisible glyph, <c>0x0020</c>.</summary>
    [<Literal>]
    let private Space = 0x0020u

    /// <summary>The period, the first punctuation advance, <c>0x002E</c>.</summary>
    [<Literal>]
    let private Period = 0x002Eu

    /// <summary>The comma, the second punctuation advance, <c>0x002C</c>.</summary>
    [<Literal>]
    let private Comma = 0x002Cu

    /// <summary>The first ASCII digit, <c>0x0030</c>.</summary>
    [<Literal>]
    let private DigitZero = 0x30u

    /// <summary>The last ASCII digit, <c>0x0039</c>.</summary>
    [<Literal>]
    let private DigitNine = 0x39u

    /// <summary>Whether a direction is forward (LTR or TTB), the predicate behind <c>HB_DIRECTION_IS_FORWARD</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for LTR and TTB.</returns>
    let private isForward direction =
        not (Direction.isBackward direction)

    /// <summary>The width rule of a space fallback type, the switch of <c>hb-ot-shape-fallback.cc:584-641</c>.</summary>
    [<Struct>]
    type private SpaceRule =
        | SpaceKeepAdvance
        | SpaceEmFraction of divisor: int
        | SpaceFourEighteenthsEm
        | SpaceFigureWidth
        | SpacePunctuationWidth
        | SpaceHalfAdvance

    /// <summary>Classifies a space fallback type into its width rule.</summary>
    /// <param name="space">The space type.</param>
    /// <returns>The rule.</returns>
    let private (|SpaceRule|) (space: SpaceFallback) : SpaceRule =
        match space with
        | SpaceFallback.NotSpace | SpaceFallback.Space -> SpaceKeepAdvance
        | SpaceFallback.SpaceEm -> SpaceEmFraction 1
        | SpaceFallback.SpaceEm2 -> SpaceEmFraction 2
        | SpaceFallback.SpaceEm3 -> SpaceEmFraction 3
        | SpaceFallback.SpaceEm4 -> SpaceEmFraction 4
        | SpaceFallback.SpaceEm5 -> SpaceEmFraction 5
        | SpaceFallback.SpaceEm6 -> SpaceEmFraction 6
        | SpaceFallback.SpaceEm16 -> SpaceEmFraction 16
        | SpaceFallback.Space4Em18 -> SpaceFourEighteenthsEm
        | SpaceFallback.SpaceFigure -> SpaceFigureWidth
        | SpaceFallback.SpacePunctuation -> SpacePunctuationWidth
        | SpaceFallback.SpaceNarrow -> SpaceHalfAdvance

    /// <summary>Finds the first ASCII digit the font maps, <c>hb-ot-shape-fallback.cc:609-618</c>'s loop.</summary>
    /// <param name="font">The font.</param>
    /// <param name="u">The next digit code point.</param>
    /// <returns>The glyph, or none when no digit maps.</returns>
    [<TailCall>]
    let rec private firstDigitGlyph (font: Font) (u: uint32) : uint32 voption =
        match u > DigitNine with
        | true -> ValueNone
        | false ->
            match Font.nominalGlyph font u with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone -> firstDigitGlyph font (u + 1u)

    /// <summary>The figure-width advance: the first mapped digit's advance, <c>hb-ot-shape-fallback.cc:608-618</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="position">The position.</param>
    /// <returns>The updated position.</returns>
    let private figureWidth (font: Font) (horizontal: bool) (position: GlyphPosition) : GlyphPosition =
        match firstDigitGlyph font DigitZero with
        | ValueNone -> position
        | ValueSome glyph ->
            match horizontal with
            | true -> { position with XAdvance = Font.hAdvance font glyph }
            | false -> { position with YAdvance = Font.vAdvance font glyph }

    /// <summary>The punctuation-width advance: the period's or comma's advance, <c>hb-ot-shape-fallback.cc:620-629</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="position">The position.</param>
    /// <returns>The updated position.</returns>
    let private punctuationWidth (font: Font) (horizontal: bool) (position: GlyphPosition) : GlyphPosition =
        let glyph =
            match Font.nominalGlyph font Period with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone -> Font.nominalGlyph font Comma
        match glyph with
        | ValueNone -> position
        | ValueSome glyph ->
            match horizontal with
            | true -> { position with XAdvance = Font.hAdvance font glyph }
            | false -> { position with YAdvance = Font.vAdvance font glyph }

    /// <summary>Computes one space glyph's adjusted advance, <c>hb-ot-shape-fallback.cc:567-641</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="invisible">The invisible glyph, zero when unset.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <param name="position">The position.</param>
    /// <returns>The updated position.</returns>
    let private spacePosition (font: Font) (horizontal: bool) (invisible: uint32) (info: GlyphInfo) (props: GlyphProps) (position: GlyphPosition) : GlyphPosition =
        let position =
            match invisible <> 0u && info.Codepoint = invisible with
            | true ->
                match horizontal with
                | true -> { position with XAdvance = font.XScale / 4 }
                | false -> { position with YAdvance = -(int64 font.YScale) / 4L |> int }
            | false -> position
        match (|SpaceRule|) (UnicodeProps.spaceFallback props) with
        | SpaceKeepAdvance -> position
        | SpaceEmFraction divisor ->
            match horizontal with
            | true -> { position with XAdvance = (int64 font.XScale + int64 (divisor / 2)) / int64 divisor |> int }
            | false -> { position with YAdvance = -((int64 font.YScale + int64 (divisor / 2)) / int64 divisor) |> int }
        | SpaceFourEighteenthsEm ->
            match horizontal with
            | true -> { position with XAdvance = int64 font.XScale * 4L / 18L |> int }
            | false -> { position with YAdvance = -(int64 font.YScale * 4L / 18L) |> int }
        | SpaceFigureWidth -> figureWidth font horizontal position
        | SpacePunctuationWidth -> punctuationWidth font horizontal position
        | SpaceHalfAdvance ->
            match horizontal with
            | true -> { position with XAdvance = position.XAdvance / 2 }
            | false -> { position with YAdvance = position.YAdvance / 2 }

    /// <summary>Adjusts the widths of space separators, <c>_hb_ot_shape_fallback_spaces</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with fallback space advances.</returns>
    let fallbackSpaces (font: Font) (run: GlyphRun) : GlyphRun =
        let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
        let positions =
            Sealed.mapi (fun index (position: GlyphPosition) ->
                let info = run.Infos[index]
                let props = run.Props[index]
                match UnicodeProps.isUnicodeSpace props && props.GlyphClass &&& GlyphPropsBits.Ligated = 0us with
                | false -> position
                | true -> spacePosition font horizontal run.Invisible info props position) (run.Positions.AsSpan())
        { run with Positions = positions }

    /// <summary>Subtracts with int saturation, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The saturating difference.</returns>
    let private saturateSub (a: int) (b: int) : int =
        int (Math.Clamp(int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Zeroes mark widths, optionally shifting their offsets first, <c>zero_mark_widths_by_gdef</c>.</summary>
    /// <param name="adjustOffsets">Whether the offsets are adjusted before zeroing.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with zeroed mark widths.</returns>
    let zeroMarkWidthsByGdef (adjustOffsets: bool) (run: GlyphRun) : GlyphRun =
        let positions =
            Sealed.mapi (fun index (position: GlyphPosition) ->
                match run.Props[index].GlyphClass &&& GlyphPropsBits.Mark <> 0us with
                | false -> position
                | true ->
                    let position =
                        match adjustOffsets with
                        | true ->
                            { position with
                                XOffset = saturateSub position.XOffset position.XAdvance
                                YOffset = saturateSub position.YOffset position.YAdvance }
                        | false -> position
                    { position with XAdvance = 0; YAdvance = 0 }) (run.Positions.AsSpan())
        { run with Positions = positions }

    /// <summary>Zeroes default-ignorable widths and the on-axis offset, <c>hb_ot_zero_width_default_ignorables</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with zeroed default ignorables.</returns>
    let zeroWidthDefaultIgnorables (run: GlyphRun) : GlyphRun =
        match run.ScratchFlags &&& ScratchFlags.HasDefaultIgnorables = ScratchFlags.Default
              || run.Flags &&& BufferFlags.PreserveDefaultIgnorables <> BufferFlags.Default
              || run.Flags &&& BufferFlags.RemoveDefaultIgnorables <> BufferFlags.Default with
        | true -> run
        | false ->
            let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
            let positions =
                Sealed.mapi (fun index (position: GlyphPosition) ->
                    match UnicodeProps.isDefaultIgnorable run.Props[index] with
                    | false -> position
                    | true ->
                        { position with
                            XAdvance = 0
                            YAdvance = 0
                            XOffset = if horizontal then 0 else position.XOffset
                            YOffset = if horizontal then position.YOffset else 0 }) (run.Positions.AsSpan())
            { run with Positions = positions }

    /// <summary>Fills the positions with the default advances, <c>hb_ot_position_default</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The cleared run.</param>
    /// <returns>The run with default advances, or a not-yet-ported vertical source.</returns>
    let private positionDefault font run =
        match Direction.isHorizontal run.SegmentProperties.Direction with
        | true ->
            let positions = Font.hAdvancePositions font (run.Infos.AsSpan())
            let defaulted = { run with Positions = positions }
            match run.ScratchFlags &&& ScratchFlags.HasSpaceFallback = ScratchFlags.Default with
            | true -> Ok defaulted
            | false -> Ok (fallbackSpaces font defaulted)
        | false ->
            let codepoints = Sealed.map (fun (info: GlyphInfo) -> info.Codepoint) (run.Infos.AsSpan())
            let advances = Font.vAdvances font (codepoints.AsSpan())
            let positions =
                Sealed.init run.Infos.Length (fun index ->
                    { XAdvance = 0; YAdvance = advances[index]; XOffset = 0; YOffset = 0; Var = 0u })
            let defaulted = { run with Positions = positions }
            match Origins.subtractVOrigins font defaulted with
            | Error error -> Error error
            | Ok subtracted ->
                match run.ScratchFlags &&& ScratchFlags.HasSpaceFallback = ScratchFlags.Default with
                | true -> Ok subtracted
                | false -> Ok (fallbackSpaces font subtracted)

    /// <summary>Runs the position stages and reverses a backward run, the composition kept callable for the
    /// equivalence property of <see cref="positionProgram"/>, <c>hb_ot_position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let internal positionComposed plan font run =
        let cleared = GlyphRun.clearPositions run
        match positionDefault font cleared with
        | Error error -> Error error
        | Ok defaulted ->
            let adjustOffsetsWhenZeroing = plan.AdjustMarkPositioningWhenZeroing && isForward run.SegmentProperties.Direction
            let started = OtLayout.positionStart defaulted
            let early =
                match plan.ZeroMarks, plan.Shaper.ZeroWidthMarks with
                | true, ZeroWidthMarksByGdefEarly -> zeroMarkWidthsByGdef adjustOffsetsWhenZeroing started
                | _ -> started
            let substituted = OtLayout.position plan font early
            let tracked =
                match plan.ApplyTrak with
                | true -> AatLayout.track font substituted
                | false -> substituted
            let late =
                match plan.ZeroMarks, plan.Shaper.ZeroWidthMarks with
                | true, ZeroWidthMarksByGdefLate -> zeroMarkWidthsByGdef adjustOffsetsWhenZeroing tracked
                | _ -> tracked
            let advanced = OtLayout.finishAdvances late
            let zeroIgnorables = zeroWidthDefaultIgnorables advanced
            let offset = OtLayout.finishOffsets font zeroIgnorables
            match OtLayout.fallbackMarkPosition plan font adjustOffsetsWhenZeroing offset with
            | Error error -> Error error
            | Ok positioned ->
                match Direction.isBackward run.SegmentProperties.Direction with
                | true -> Ok (GlyphRun.reverse positioned)
                | false -> Ok positioned

    /// <summary>The mask that clears a position's attach chain and type, <c>GPOS::position_start</c>.</summary>
    [<Literal>]
    let private AttachClearMask = 0xFF000000u

    /// <summary>Rebuilds the glyph run a whole-run stage reads from the interpreter's run fields.</summary>
    /// <param name="input">The run fields.</param>
    /// <param name="props">The segment properties, captured when the program was built.</param>
    /// <returns>The run.</returns>
    let private runOfInput (input: RunInput) (props: SegmentProperties) =
        { GlyphRun.empty with
            Infos = input.Infos
            Props = input.Props
            Positions = input.Positions
            HavePositions = input.HavePositions
            MaxLen = input.MaxLen
            MaxOps = input.MaxOps
            Successful = input.Successful
            ClusterLevel = input.ClusterLevel
            Flags = input.Flags
            Serial = uint32 input.Serial
            RandomState = input.RandomState
            ScratchFlags = input.ScratchFlags
            SegmentProperties = props }

    /// <summary>Builds the run fields a whole-run stage returns from the run it produced.</summary>
    /// <param name="next">The run the stage produced.</param>
    /// <param name="unchanged">Whether the stage returned its input unchanged.</param>
    /// <returns>The run fields.</returns>
    let private resultOf (next: GlyphRun) (unchanged: bool) =
        { Infos = next.Infos
          Props = next.Props
          Positions = next.Positions
          HavePositions = next.HavePositions
          MaxOps = next.MaxOps
          Successful = next.Successful
          Unchanged = unchanged
          Serial = byte next.Serial
          RandomState = next.RandomState
          ScratchFlags = next.ScratchFlags }

    /// <summary>A stage that clears the positions and turns them on, <c>hb_buffer_clear_positions</c>.</summary>
    [<Struct>]
    type private ClearPositionsStage =
        interface Sealed.IStage with
            member _.Run (sink: byref<Sealed.StageSink>) = Sealed.clearPositions &sink

    /// <summary>A stage that writes each glyph's default horizontal advance, <c>hb_ot_position_default</c>'s
    /// horizontal branch.</summary>
    [<Struct>]
    type private DefaultAdvanceStage =
        val Font: Font
        new (font) = { Font = font }
        interface IPositionMap with
            member this.Map (info, _, _) =
                { XAdvance = Font.hAdvance this.Font info.Codepoint
                  YAdvance = 0
                  XOffset = 0
                  YOffset = 0
                  Var = 0u }
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink this

    /// <summary>A stage that adjusts the advances of space separators, <c>_hb_ot_shape_fallback_spaces</c>. A no-op
    /// when the run has no space fallback.</summary>
    [<Struct>]
    type private FallbackSpacesStage =
        val Font: Font
        val Horizontal: bool
        val Invisible: uint32
        val Enabled: bool
        new (font, horizontal, invisible, enabled) = { Font = font; Horizontal = horizontal; Invisible = invisible; Enabled = enabled }
        interface IPositionMap with
            member this.Map (info, props, position) =
                match this.Enabled && UnicodeProps.isUnicodeSpace props && props.GlyphClass &&& GlyphPropsBits.Ligated = 0us with
                | false -> position
                | true -> spacePosition this.Font this.Horizontal this.Invisible info props position
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink this

    /// <summary>A stage that clears each position's attach chain and type, <c>GPOS::position_start</c>.</summary>
    [<Struct>]
    type private PositionStartStage =
        interface IPositionMap with
            member _.Map (_, _, position) = { position with Var = position.Var &&& AttachClearMask }
        interface Sealed.IStage with
            member _.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink (PositionStartStage())

    /// <summary>A stage that zeroes mark widths, optionally shifting their offsets first,
    /// <c>zero_mark_widths_by_gdef</c>. A no-op when the plan does not zero marks this way.</summary>
    [<Struct>]
    type private ZeroMarkWidthsStage =
        val AdjustOffsets: bool
        val Enabled: bool
        new (adjustOffsets, enabled) = { AdjustOffsets = adjustOffsets; Enabled = enabled }
        interface IPositionMap with
            member this.Map (_, props, position) =
                match this.Enabled && props.GlyphClass &&& GlyphPropsBits.Mark <> 0us with
                | false -> position
                | true ->
                    let position =
                        match this.AdjustOffsets with
                        | true ->
                            { position with
                                XOffset = saturateSub position.XOffset position.XAdvance
                                YOffset = saturateSub position.YOffset position.YAdvance }
                        | false -> position
                    { position with XAdvance = 0; YAdvance = 0 }
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink this

    /// <summary>A whole-run stage that applies the GPOS lookups, the legacy kern machine and the tracking,
    /// <c>hb_ot_map_t::position</c> plus <c>hb_aat_layout_track</c>. The GPOS lookups are not ported yet, so on a
    /// face without kern or trak the stage changes nothing and the interpreter keeps its working copy.</summary>
    [<Struct>]
    type private GposKernTrackStage =
        val Plan: OtShapePlan
        val Font: Font
        val Props: SegmentProperties
        new (plan, font, props) = { Plan = plan; Font = font; Props = props }
        interface IRunTransform with
            member this.Transform (input) =
                let run = runOfInput input this.Props
                let next = OtLayout.position this.Plan this.Font run
                let next = match this.Plan.ApplyTrak with | true -> AatLayout.track this.Font next | false -> next
                resultOf next (obj.ReferenceEquals(next, run))
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.runTransform &sink this

    /// <summary>A stage that zeroes default-ignorable widths and the on-axis offset,
    /// <c>hb_ot_zero_width_default_ignorables</c>. A no-op when the run has no default ignorables or the buffer
    /// flags preserve or remove them.</summary>
    [<Struct>]
    type private ZeroIgnorablesStage =
        val Horizontal: bool
        val Enabled: bool
        new (horizontal, enabled) = { Horizontal = horizontal; Enabled = enabled }
        interface IPositionMap with
            member this.Map (_, props, position) =
                match this.Enabled && UnicodeProps.isDefaultIgnorable props with
                | false -> position
                | true ->
                    { position with
                        XAdvance = 0
                        YAdvance = 0
                        XOffset = if this.Horizontal then 0 else position.XOffset
                        YOffset = if this.Horizontal then position.YOffset else 0 }
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink this

    /// <summary>A whole-run stage that propagates attachment offsets and applies the slant correction,
    /// <c>GPOS::position_finish_offsets</c>.</summary>
    [<Struct>]
    type private FinishOffsetsStage =
        val Font: Font
        val Props: SegmentProperties
        new (font, props) = { Font = font; Props = props }
        interface IRunTransform with
            member this.Transform (input) =
                let run = runOfInput input this.Props
                let next = GposApply.finishOffsets this.Font run
                resultOf next (obj.ReferenceEquals(next, run))
        interface Sealed.IStage with
            member this.Run (sink: byref<Sealed.StageSink>) = Sealed.runTransform &sink this

    /// <summary>Builds the horizontal positioning program: the position stages folded into one working copy, with
    /// the GPOS/kern/track lookups and the attachment-offset finish as whole-run stages inside it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The program.</returns>
    let private horizontalProgram (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let adjustOffsets = plan.AdjustMarkPositioningWhenZeroing && isForward run.SegmentProperties.Direction
        let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
        let early = plan.ZeroMarks && plan.Shaper.ZeroWidthMarks = ZeroWidthMarksByGdefEarly
        let late = plan.ZeroMarks && plan.Shaper.ZeroWidthMarks = ZeroWidthMarksByGdefLate
        let zeroIgnorables =
            run.ScratchFlags &&& ScratchFlags.HasDefaultIgnorables <> ScratchFlags.Default
            && run.Flags &&& BufferFlags.PreserveDefaultIgnorables = BufferFlags.Default
            && run.Flags &&& BufferFlags.RemoveDefaultIgnorables = BufferFlags.Default
        let spaces = run.ScratchFlags &&& ScratchFlags.HasSpaceFallback <> ScratchFlags.Default
        Sealed.Then(
            ClearPositionsStage(),
            Sealed.Then(
                DefaultAdvanceStage(font),
                Sealed.Then(
                    FallbackSpacesStage(font, horizontal, run.Invisible, spaces),
                    Sealed.Then(
                        PositionStartStage(),
                        Sealed.Then(
                            ZeroMarkWidthsStage(adjustOffsets, early),
                            Sealed.Then(
                                GposKernTrackStage(plan, font, run.SegmentProperties),
                                Sealed.Then(
                                    ZeroMarkWidthsStage(adjustOffsets, late),
                                    Sealed.Then(
                                        ZeroIgnorablesStage(horizontal, zeroIgnorables),
                                        FinishOffsetsStage(font, run.SegmentProperties)))))))))

    /// <summary>Runs the positioning stages as one program, <c>hb_ot_position</c>. A vertical run uses the
    /// composition, which is the path that can report unported vertical origins.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let positionProgram plan font run =
        match Direction.isHorizontal run.SegmentProperties.Direction with
        | false -> positionComposed plan font run
        | true ->
            let adjustOffsets = plan.AdjustMarkPositioningWhenZeroing && isForward run.SegmentProperties.Direction
            let input =
                { Infos = run.Infos
                  Props = run.Props
                  Positions = run.Positions
                  HavePositions = run.HavePositions
                  MaxLen = run.MaxLen
                  MaxOps = run.MaxOps
                  Successful = run.Successful
                  ClusterLevel = run.ClusterLevel
                  Flags = run.Flags
                  Serial = byte run.Serial
                  RandomState = run.RandomState
                  ScratchFlags = run.ScratchFlags }
            let result = Sealed.runStages input (horizontalProgram plan font run)
            let positioned =
                match result.Unchanged with
                | true -> run
                | false ->
                    { run with
                        Infos = result.Infos
                        Props = result.Props
                        Positions = result.Positions
                        HavePositions = result.HavePositions
                        MaxOps = result.MaxOps
                        Successful = result.Successful
                        Serial = uint32 result.Serial
                        RandomState = result.RandomState
                        ScratchFlags = result.ScratchFlags }
            match OtLayout.fallbackMarkPosition plan font adjustOffsets positioned with
            | Error error -> Error error
            | Ok p ->
                match Direction.isBackward run.SegmentProperties.Direction with
                | true -> Ok (GlyphRun.reverse p)
                | false -> Ok p

    /// <summary>Runs the position stages and reverses a backward run, <c>hb_ot_position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let position plan font run =
        positionProgram plan font run
