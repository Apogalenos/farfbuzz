namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>The noncontextual morx/mort subtable, <c>NoncontextualSubtable</c> (src/hb-aat-layout-morx-table.hh:640-700).</summary>
[<RequireQualifiedAccess>]
module internal MorxNoncontextual =
    /// <summary>Finds the range covering a cluster, walking down then up, the noncontextual loop's range walk
    /// (src/hb-aat-layout-morx-table.hh:656-668).</summary>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="index">The current range index.</param>
    /// <param name="cluster">The glyph cluster.</param>
    /// <returns>The range index.</returns>
    [<TailCall>]
    let rec private rangeFor (ranges: ReadOnlySpan<RangeFlags>) (index: int) (cluster: uint32) =
        match index > 0 && cluster < ranges[index].ClusterFirst with
        | true -> rangeFor ranges (index - 1) cluster
        | false ->
            match index < ranges.Length - 1 && cluster > ranges[index].ClusterLast with
            | true -> rangeFor ranges (index + 1) cluster
            | false -> index

    /// <summary>Counts the glyphs a lookup would replace, <c>NoncontextualSubtable::apply</c>'s scan.</summary>
    /// <param name="lookup">The substitute lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="rangeIndex">The current range index.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="count">The replacements counted so far.</param>
    /// <returns>The replacement count.</returns>
    [<TailCall>]
    let rec private countRepls
        (lookup: Lookup) (numGlyphs: int) (ranges: ReadOnlySpan<RangeFlags>) (rangeCount: int) (subFlags: uint32)
        (infos: ReadOnlySpan<GlyphInfo>) (rangeIndex: int) (index: int) (count: int) =
        match index >= infos.Length with
        | true -> count
        | false ->
            let rangeIndex =
                match rangeCount > 1 with
                | true -> rangeFor ranges rangeIndex infos[index].Cluster
                | false -> rangeIndex
            let enabled = rangeCount <= 1 || (ranges[rangeIndex].Flags &&& subFlags <> 0u)
            let found =
                match enabled with
                | false -> false
                | true ->
                    match AatLookup.valueAt lookup infos[index].Codepoint numGlyphs with
                    | ValueSome _ -> true
                    | ValueNone -> false
            countRepls lookup numGlyphs ranges rangeCount subFlags infos rangeIndex (index + 1) (match found with | true -> count + 1 | false -> count)

    /// <summary>Builds the replaced run: new infos, new props, the written glyphs and whether a glyph was deleted.</summary>
    /// <param name="lookup">The substitute lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="rangeIndex">The current range index.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="infoBuilder">The new infos.</param>
    /// <param name="propsBuilder">The new props.</param>
    /// <param name="writtenBuilder">The written glyphs.</param>
    /// <param name="hasDeleted">Whether any glyph was deleted.</param>
    /// <returns>The new infos, props, written glyphs and the deleted flag.</returns>
    [<TailCall>]
    let rec private buildRepls
        (lookup: Lookup) (numGlyphs: int) (ranges: ReadOnlySpan<RangeFlags>) (rangeCount: int) (subFlags: uint32)
        (context: AatContext) (usingSet: bool) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
        (rangeIndex: int) (index: int) (infoBuilder: PointBuilder<GlyphInfo>) (propsBuilder: PointBuilder<GlyphProps>)
        (writtenBuilder: PointBuilder<uint32>) (hasDeleted: bool) =
        match index >= infos.Length with
        | true -> struct (PointBuilder.publish infoBuilder, PointBuilder.publish propsBuilder, PointBuilder.publish writtenBuilder, hasDeleted)
        | false ->
            let rangeIndex =
                match rangeCount > 1 with
                | true -> rangeFor ranges rangeIndex infos[index].Cluster
                | false -> rangeIndex
            let info = infos[index]
            let glyphProps = props[index]
            let replacement =
                match rangeCount <= 1 || (ranges[rangeIndex].Flags &&& subFlags <> 0u) with
                | true -> AatLookup.valueAt lookup info.Codepoint numGlyphs
                | false -> ValueNone
            match replacement with
            | ValueNone ->
                buildRepls lookup numGlyphs ranges rangeCount subFlags context usingSet infos props rangeIndex (index + 1)
                    (PointBuilder.append infoBuilder info) (PointBuilder.append propsBuilder glyphProps) writtenBuilder hasDeleted
            | ValueSome offset ->
                let glyph = AatLookup.valueOf lookup.Table.Span offset lookup.ValueSize
                let newInfo = { info with Codepoint = glyph }
                let deleted = glyph = AatLookup.DeletedGlyph
                let newProps =
                    let marked = match deleted with | true -> AatApply.deletedProps glyphProps | false -> glyphProps
                    match context.Gdef.HasGlyphClasses with
                    | true -> { marked with GlyphClass = Gdef.glyphProps context.Gdef glyph }
                    | false -> marked
                let written =
                    match usingSet with
                    | true -> PointBuilder.append writtenBuilder glyph
                    | false -> writtenBuilder
                buildRepls lookup numGlyphs ranges rangeCount subFlags context usingSet infos props rangeIndex (index + 1)
                    (PointBuilder.append infoBuilder newInfo) (PointBuilder.append propsBuilder newProps) written (hasDeleted || deleted)

    /// <summary>Applies the noncontextual subtable in place, <c>NoncontextualSubtable::apply</c>. A run the lookup
    /// does not change returns unchanged, allocating nothing.</summary>
    /// <param name="lookup">The substitute lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run and the glyphs written, when the glyph set is in use.</returns>
    let apply (lookup: Lookup) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (subFlags: uint32) (context: AatContext) (usingSet: bool) (run: GlyphRun) =
        let rangeSpan = ranges.AsSpan()
        let count = countRepls lookup numGlyphs rangeSpan ranges.Length subFlags (run.Infos.AsSpan()) 0 0 0
        match count with
        | 0 -> struct (run, ImmutableArray.Empty)
        | _ ->
            let struct (infos, props, written, hasDeleted) =
                buildRepls lookup numGlyphs rangeSpan ranges.Length subFlags context usingSet (run.Infos.AsSpan()) (run.Props.AsSpan()) 0 0
                    (PointBuilder.create()) (PointBuilder.create()) (PointBuilder.create()) false
            let scratch =
                match hasDeleted with
                | true -> run.ScratchFlags ||| AatApply.aatHasDeleted
                | false -> run.ScratchFlags
            struct ({ run with Infos = infos; Props = props; ScratchFlags = scratch }, written)

/// <summary>The decisions of a noncontextual subtable as <see cref="INoncontextualDecisions"/>: the replacement array compiled with
/// the face, the lookup for the glyphs beyond it, and the property logic of <see cref="MorxNoncontextual"/>.</summary>
[<Struct; NoComparison; NoEquality; System.Runtime.CompilerServices.IsReadOnly>]
type internal NoncontextualDecisions =
    { Replacements: ImmutableArray<uint32>
      Lookup: Lookup
      NumGlyphs: int
      Ranges: ImmutableArray<RangeFlags>
      SubFlags: uint32
      Context: AatContext }

    interface INoncontextualDecisions with
        member this.RangeCount = this.Ranges.Length
        member this.AdvanceRange (index, cluster) = Driver.rangeFor (this.Ranges.AsSpan()) index cluster
        member this.RangeEnabled index = this.Ranges[index].Flags &&& this.SubFlags <> 0u
        member this.Replacement glyph =
            match glyph < uint32 this.Replacements.Length with
            | true ->
                let replacement = this.Replacements[int glyph]
                struct (replacement <> Morx.NoReplacement, replacement)
            | false ->
                match AatLookup.valueAt this.Lookup glyph this.NumGlyphs with
                | ValueSome offset -> struct (true, AatLookup.valueOf this.Lookup.Table.Span offset this.Lookup.ValueSize)
                | ValueNone -> struct (false, 0u)
        member this.Props (glyph, props) =
            let marked = match glyph = AatLookup.DeletedGlyph with | true -> AatApply.deletedProps props | false -> props
            match this.Context.Gdef.HasGlyphClasses with
            | true -> { marked with GlyphClass = Gdef.glyphProps this.Context.Gdef glyph }
            | false -> marked

/// <summary>Applying the noncontextual subtable on the kernel that writes the replacements in one loop, or on the reference
/// that builds the run again.</summary>
[<RequireQualifiedAccess>]
module internal MorxNoncontextualDirect =
    /// <summary>Applies a noncontextual subtable, <c>NoncontextualSubtable::apply</c>.</summary>
    /// <param name="runner">Which implementation applies it.</param>
    /// <param name="replacements">The replacements compiled with the face, or none to read the lookup.</param>
    /// <param name="lookup">The substitute lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run and the glyphs written, when the glyph set is in use.</returns>
    let run (runner: MachineRunner) (replacements: ImmutableArray<uint32> voption) (lookup: Lookup) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (subFlags: uint32) (context: AatContext) (usingSet: bool) (run: GlyphRun) =
        match runner with
        | Reference -> MorxNoncontextual.apply lookup numGlyphs ranges subFlags context usingSet run
        | Kernel ->
            let decisions =
                { NoncontextualDecisions.Replacements = match replacements with | ValueSome compiled -> compiled | ValueNone -> ImmutableArray.Empty
                  Lookup = lookup
                  NumGlyphs = numGlyphs
                  Ranges = ranges
                  SubFlags = subFlags
                  Context = context }
            let struct (infos, props, written, hasDeleted, changed) = Sealed.runNoncontextualPass decisions run.Infos run.Props usingSet
            match changed with
            | false -> struct (run, ImmutableArray.Empty)
            | true ->
                let scratch = match hasDeleted with | true -> run.ScratchFlags ||| AatApply.aatHasDeleted | false -> run.ScratchFlags
                struct ({ run with Infos = infos; Props = props; ScratchFlags = scratch }, written)

    /// <summary>Applies a noncontextual subtable to the working set of a line, <c>NoncontextualSubtable::apply</c>, the glyphs
    /// written going to the session's collection.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="replacements">The replacements compiled with the face, or none to read the lookup.</param>
    /// <param name="lookup">The substitute lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="context">The apply context.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal runSession (session: Sealed.AatSession) (replacements: ImmutableArray<uint32> voption) (lookup: Lookup) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (subFlags: uint32) (context: AatContext) =
        let decisions =
            { NoncontextualDecisions.Replacements = match replacements with | ValueSome compiled -> compiled | ValueNone -> ImmutableArray.Empty
              Lookup = lookup
              NumGlyphs = numGlyphs
              Ranges = ranges
              SubFlags = subFlags
              Context = context }
        Sealed.aatSessionNoncontextual session decisions
