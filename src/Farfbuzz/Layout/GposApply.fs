namespace Farfbuzz

open System
open Farfbuzz.Var

/// <summary>The GPOS position start and offset finish of <c>GPOS::position_start</c> and <c>position_finish_offsets</c>.</summary>
[<RequireQualifiedAccess>]
module internal GposApply =
    /// <summary>The mask that clears the attach chain and type, the low three bytes of <c>var</c>.</summary>
    [<Literal>]
    let private AttachClearMask = 0xFF000000u

    /// <summary>Whether a direction is forward, <c>HB_DIRECTION_IS_FORWARD</c>: left to right and top to bottom, and no other, the
    /// invalid direction included.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for a forward direction.</returns>
    let private isForward direction =
        match direction with
        | Ltr -> true
        | Ttb -> true
        | Rtl -> false
        | Btt -> false
        | Invalid -> false

    /// <summary>Corrects the horizontal offset of a position for the slant of the font, <c>position_finish_offsets</c>'s last loop.</summary>
    /// <param name="slant">The slant.</param>
    /// <param name="position">The position.</param>
    /// <returns>The position.</returns>
    let private slanted (slant: float32) (position: GlyphPosition) =
        match position.YOffset <> 0 with
        | false -> position
        | true -> { position with XOffset = Fixed.clampToInt32 (double position.XOffset + double (Fixed.roundf (slant * float32 position.YOffset))) }

    /// <summary>Clears the attach chain and type of every position, <c>GPOS::position_start</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the attachments cleared.</returns>
    let positionStart (run: GlyphRun) : GlyphRun =
        match run.HavePositions with
        | false -> run
        | true ->
            let positions = Sealed.mapIfChanged (fun (position: GlyphPosition) -> { position with Var = position.Var &&& AttachClearMask }) run.Positions
            { run with Positions = positions }

    /// <summary>Propagates attachment offsets and applies the slant correction, <c>GPOS::position_finish_offsets</c>: the offsets are
    /// propagated in place along the chains, in upstream's order, when the run has a GPOS attachment, and the offsets of a
    /// horizontal run of a slanted font are corrected.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the offsets finished, the run itself when there is nothing to do.</returns>
    let finishOffsets (font: Font) (run: GlyphRun) : GlyphRun =
        match run.HavePositions with
        | false -> run
        | true ->
            let direction = run.SegmentProperties.Direction
            let horizontal = Direction.isHorizontal direction
            let attached =
                match run.ScratchFlags &&& ScratchFlags.HasGposAttachment <> ScratchFlags.Default with
                | true -> Sealed.propagateAttachments run.Positions (isForward direction) horizontal
                | false -> run.Positions
            let finished =
                match font.SlantXY <> 0f && horizontal with
                | true -> Sealed.mapIfChanged (slanted font.SlantXY) attached
                | false -> attached
            match finished.Equals run.Positions with
            | true -> run
            | false -> { run with Positions = finished }
