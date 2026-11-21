namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Farfbuzz

/// <summary>The decisions of a ligature subtable as <see cref="ILigatureDecisions{E}"/>: the action, component and ligature
/// reads and the property logic of <see cref="MorxLigature"/>, for the kernel that runs the action list directly.</summary>
[<Struct; NoComparison; NoEquality; IsReadOnly>]
type internal LigatureDecisions =
    { Kind: LigatureKind
      CollectWritten: bool }

    interface ILigatureDecisions<Entry> with
        member this.Collect = this.CollectWritten
        member this.Performs entry = MorxLigature.performAction this.Kind.Extended entry
        member _.SetsComponent entry = entry.Flags &&& MorxLigature.SetComponentFlag <> 0us
        member this.ActionAt entry = MorxLigatureReads.actionStart this.Kind.Extended this.Kind.LigActionAt entry
        member this.ReadAction at = MorxLigatureReads.action this.Kind.Body at
        member this.ReadComponent (glyph, action) = MorxLigatureReads.componentWord this.Kind.Extended this.Kind.Body this.Kind.ComponentAt glyph action
        member this.ReadLigature index = MorxLigatureReads.ligature this.Kind.Extended this.Kind.Body this.Kind.LigatureAt index
        member this.Props (glyph, props) = AatApply.propsFor this.Kind.Context glyph props

/// <summary>Running one ligature subtable over a run, on the kernel that runs the action list straight on the working copy, or
/// on the reference interpreter.</summary>
[<RequireQualifiedAccess>]
module internal MorxLigatureDirect =
    /// <summary>Runs a ligature subtable's state machine.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let run (runner: MachineRunner) (dense: DenseMachine voption) (config: DriverConfig) (kind: LigatureKind) (collect: bool) (run: GlyphRun) =
        let decisions = { LigatureDecisions.Kind = kind; CollectWritten = collect }
        match runner, dense with
        | Kernel, ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = true; NullMask = 0x8000us }
            let struct (result, written, deleted) = Sealed.runLigaturePass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Kernel, ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = true; NullMask = 0x8000us }
            let struct (result, written, deleted) = Sealed.runLigaturePass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Reference, _ ->
            let struct (applied, context) = AatKernel.run Reference ValueNone config kind 0x8000us true (MorxLigature.seed collect) run
            struct (applied, PointBuilder.publish context.Written, context.HasDeleted)

    /// <summary>Runs a ligature subtable's state machine over the working set of a line, the glyphs written going to the
    /// session's collection.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal runSession (session: Sealed.AatSession) (dense: DenseMachine voption) (config: DriverConfig) (kind: LigatureKind) (collect: bool) =
        let decisions = { LigatureDecisions.Kind = kind; CollectWritten = collect }
        match dense with
        | ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = true; NullMask = 0x8000us }
            Sealed.aatSessionLigature session tables decisions
        | ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = true; NullMask = 0x8000us }
            Sealed.aatSessionLigature session tables decisions
