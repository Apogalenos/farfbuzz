namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>The decisions of a contextual substitution subtable as <see cref="IContextualDecisions{E}"/>: the substitution
/// reads and the property logic of <see cref="MorxContextual"/>, for the kernel that applies them directly.</summary>
[<Struct; NoComparison; NoEquality; IsReadOnly>]
type internal ContextualDecisions =
    { Kind: ContextualKind
      CollectWritten: bool
      Substitutions: ImmutableArray<uint32> } // the substitutions compiled with the face, or empty to read the lookups

    interface IContextualDecisions<Entry> with
        member this.Collect = this.CollectWritten
        member this.Extended = this.Kind.Extended
        member this.Actionable entry = MorxContextualEntry.isActionable this.Kind.Extended entry
        member _.SetsMark entry = entry.Flags &&& 0x8000us <> 0us
        member _.MarkIndex entry = entry.Data0
        member _.CurrentIndex entry = entry.Data1
        member this.Replacement (index, glyph) =
            let cell = int64 index * int64 this.Kind.NumGlyphs + int64 glyph
            match this.Substitutions.Length > 0 && index <> 0xFFFFus && glyph < uint32 this.Kind.NumGlyphs && cell < int64 this.Substitutions.Length with
            | true ->
                let replacement = this.Substitutions[int cell]
                struct (replacement <> Morx.NoReplacement, replacement)
            | false ->
                match MorxContextual.resolve this.Kind.Extended this.Kind.Body this.Kind.SubstitutionTablesAt this.Kind.NumGlyphs index glyph with
                | ValueSome replacement -> struct (true, replacement)
                | ValueNone -> struct (false, 0u)
        member this.Props (glyph, props) = AatApply.propsFor this.Kind.Context glyph props

/// <summary>Running one contextual substitution subtable over a run, on the kernel that writes the substitutions straight
/// into the working copy, or on the reference interpreter.</summary>
[<RequireQualifiedAccess>]
module internal MorxContextualDirect =
    /// <summary>Runs a contextual subtable's state machine.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let run (runner: MachineRunner) (dense: DenseMachine voption) (config: DriverConfig) (kind: ContextualKind) (collect: bool) (run: GlyphRun) =
        let decisions = { ContextualDecisions.Kind = kind; CollectWritten = collect; Substitutions = ImmutableArray.Empty }
        match runner, dense with
        | Kernel, ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = false; NullMask = 0us }
            let struct (result, written, deleted) = Sealed.runContextualPass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Kernel, ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = false; NullMask = 0us }
            let struct (result, written, deleted) = Sealed.runContextualPass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Reference, _ ->
            let struct (applied, context) = AatKernel.run Reference ValueNone config kind 0x8000us false (MorxContextual.seed collect) run
            struct (applied, PointBuilder.publish context.Written, context.HasDeleted)

    /// <summary>Runs a contextual subtable's state machine over the working set of a line, the glyphs written going to the
    /// session's collection.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="substitutions">The substitutions compiled with the face, or empty to read the lookups.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal runSession (session: Sealed.AatSession) (dense: DenseMachine voption) (config: DriverConfig) (kind: ContextualKind) (substitutions: ImmutableArray<uint32>) =
        let decisions = { ContextualDecisions.Kind = kind; CollectWritten = false; Substitutions = substitutions }
        match dense with
        | ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = false; NullMask = 0us }
            Sealed.aatSessionContextual session tables decisions
        | ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = false; NullMask = 0us }
            Sealed.aatSessionContextual session tables decisions
