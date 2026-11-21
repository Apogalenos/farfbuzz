namespace Farfbuzz

open System
open Farfbuzz.Var

/// <summary>The hinting and variation Device deltas of <c>src/hb-ot-layout-common.hh</c>, which need a font.</summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Device =
    /// <summary>Returns the horizontal hinting delta in user space, <c>get_x_delta</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="device">The device.</param>
    /// <returns>The delta.</returns>
    let xDelta (font: Font) (device: Device) : int =
        match device with
        | NoDevice -> 0
        | HintingDevice (startSize, endSize, format, deltas) -> DeviceTable.hintingDelta font.XPpem font.XScale startSize endSize format deltas
        | VariationIndex (outer, inner) -> VariationDevice.xDelta font ((uint32 outer <<< 16) ||| uint32 inner)

    /// <summary>Returns the vertical hinting delta in user space, <c>get_y_delta</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="device">The device.</param>
    /// <returns>The delta.</returns>
    let yDelta (font: Font) (device: Device) : int =
        match device with
        | NoDevice -> 0
        | HintingDevice (startSize, endSize, format, deltas) -> DeviceTable.hintingDelta font.YPpem font.YScale startSize endSize format deltas
        | VariationIndex (outer, inner) -> VariationDevice.yDelta font ((uint32 outer <<< 16) ||| uint32 inner)

    /// <summary>Returns the horizontal delta as a float, <c>get_x_delta_f</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="device">The device.</param>
    /// <returns>The delta; a variation device keeps its fractional value.</returns>
    let xDeltaF (font: Font) (device: Device) : float32 =
        match device with
        | NoDevice -> 0.0f
        | HintingDevice _ -> float32 (xDelta font device)
        | VariationIndex (outer, inner) -> VariationDevice.xDeltaF font ((uint32 outer <<< 16) ||| uint32 inner)

    /// <summary>Returns the vertical delta as a float, <c>get_y_delta_f</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="device">The device.</param>
    /// <returns>The delta; a variation device keeps its fractional value.</returns>
    let yDeltaF (font: Font) (device: Device) : float32 =
        match device with
        | NoDevice -> 0.0f
        | HintingDevice _ -> float32 (yDelta font device)
        | VariationIndex (outer, inner) -> VariationDevice.yDeltaF font ((uint32 outer <<< 16) ||| uint32 inner)
