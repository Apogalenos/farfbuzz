namespace Farfbuzz.Aat

open System
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>Where the contextual transition is within its substitution micro-program.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ContextualPhase =
    | Idle
    | MarkReplace
    | CurrentReplace
    | Done

/// <summary>The contextual transition context: the marked glyph, the pending substitution edits and the written glyphs.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ContextualContext =
    { MarkSet: bool
      Mark: int
      CollectWritten: bool
      Written: PointBuilder<uint32>
      HasDeleted: bool
      Phase: ContextualPhase
      NextMarkSet: bool
      NextMark: int
      MarkReplacement: uint32
      HasMarkReplacement: bool
      CurrentReplacement: uint32
      CurrentTarget: int
      HasCurrentReplacement: bool }

/// <summary>Reading and applying the contextual morx/mort subtable (src/hb-aat-layout-morx-table.hh:197-382).</summary>
[<RequireQualifiedAccess>]
module internal MorxContextual =
    /// <summary>The <c>SetMark</c> entry bit, which marks the current glyph.</summary>
    [<Literal>]
    let private SetMarkFlag = 0x8000us

    /// <summary>The initial contextual context.</summary>
    /// <param name="collectWritten">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>The context.</returns>
    let seed collectWritten =
        { MarkSet = false
          Mark = 0
          CollectWritten = collectWritten
          Written = PointBuilder.create ()
          HasDeleted = false
          Phase = Idle
          NextMarkSet = false
          NextMark = 0
          MarkReplacement = 0u
          HasMarkReplacement = false
          CurrentReplacement = 0u
          CurrentTarget = 0
          HasCurrentReplacement = false }

    /// <summary>Resolves a mort substitution at a signed word offset, <c>wordOffsetToIndex</c>
    /// (src/hb-aat-layout-morx-table.hh:270-279, src/hb-aat-layout-common.hh:1204-1236). A zero value is no
    /// replacement.</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="index">The signed 16-bit word index.</param>
    /// <param name="glyph">The glyph code point.</param>
    /// <returns>The replacement, or none.</returns>
    let private mortReplacement (body: ReadOnlySpan<byte>) subsAt (index: uint16) glyph =
        let offset = int64 (int (uint32 (int (int16 index)) + glyph))
        let byteOffset = offset * 2L
        match byteOffset >= int64 subsAt && byteOffset + 2L <= int64 body.Length with
        | false -> ValueNone
        | true ->
            match Binary.u16 body (int byteOffset) with
            | 0us -> ValueNone
            | value -> ValueSome (uint32 value)

    /// <summary>Resolves a morx substitution through the lookup list, <c>subs[index].get_value</c>
    /// (src/hb-aat-layout-morx-table.hh:259-266).</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="index">The lookup index.</param>
    /// <param name="glyph">The glyph code point.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The replacement, or none.</returns>
    let private morxReplacement (body: ReadOnlyMemory<byte>) subsAt (index: uint16) glyph numGlyphs =
        Morx.substitutionAt body subsAt index glyph numGlyphs

    /// <summary>Resolves one entry's mark or current substitution.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="index">The mark or current index word.</param>
    /// <param name="glyph">The glyph code point.</param>
    /// <returns>The replacement, or none.</returns>
    let internal resolve extended (body: ReadOnlyMemory<byte>) subsAt numGlyphs (index: uint16) glyph =
        match extended with
        | true -> morxReplacement body subsAt index glyph numGlyphs
        | false -> mortReplacement body.Span subsAt index glyph

    /// <summary>Emits one in-place substitution edit, recording the written glyph and deletion (src/hb-aat-layout-common.hh:260-275).</summary>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <param name="index">The input index to rewrite.</param>
    /// <param name="glyph">The replacement glyph.</param>
    /// <returns>The edit and the updated context.</returns>
    let private replacementEdit (applyContext: AatContext) (cursor: inref<RunCursor>) (context: ContextualContext) index glyph =
        let props = AatApply.propsFor applyContext glyph cursor.InputProps[index]
        let written = if context.CollectWritten then PointBuilder.append context.Written glyph else context.Written
        let deleted = context.HasDeleted || glyph = AatLookup.DeletedGlyph
        struct (Emission.Emit (RunEdit.SetInputGlyph (index, glyph, props)), { context with Written = written; HasDeleted = deleted })

    /// <summary>One step of the contextual transition micro-program (src/hb-aat-layout-morx-table.hh:247-322).</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The next edit and context.</returns>
    let transition extended (body: ReadOnlyMemory<byte>) subsAt numGlyphs (applyContext: AatContext)
        (cursor: inref<RunCursor>) (entry: Entry) (context: ContextualContext) =
        match context.Phase with
        | MarkReplace ->
            let struct (emission, next) = replacementEdit applyContext &cursor context context.Mark context.MarkReplacement
            struct (emission, { next with Phase = (if context.HasCurrentReplacement then CurrentReplace else Done) })
        | CurrentReplace ->
            let struct (emission, next) = replacementEdit applyContext &cursor context context.CurrentTarget context.CurrentReplacement
            struct (emission, { next with Phase = Done })
        | Done ->
            struct (Emission.Complete, { context with MarkSet = context.NextMarkSet; Mark = context.NextMark; Phase = Idle })
        | Idle ->
            match cursor.Index >= cursor.Input.Length && not context.MarkSet with
            | true -> struct (Emission.Complete, context)
            | false ->
                let markGlyph = cursor.Input[context.Mark].Codepoint
                let markReplacement = resolve extended body subsAt numGlyphs entry.Data0 markGlyph
                let currentTarget = min cursor.Index (cursor.Input.Length - 1)
                let currentGlyph =
                    match markReplacement, currentTarget = context.Mark with
                    | ValueSome value, true -> value
                    | _ -> cursor.Input[currentTarget].Codepoint
                let currentReplacement = resolve extended body subsAt numGlyphs entry.Data1 currentGlyph
                let nextMarkSet = entry.Flags &&& SetMarkFlag <> 0us
                let resolved =
                    { context with
                        NextMarkSet = (if nextMarkSet then true else context.MarkSet)
                        NextMark = (if nextMarkSet then cursor.Index else context.Mark)
                        MarkReplacement = ValueOption.defaultValue 0u markReplacement
                        HasMarkReplacement = markReplacement.IsSome
                        CurrentReplacement = ValueOption.defaultValue 0u currentReplacement
                        CurrentTarget = currentTarget
                        HasCurrentReplacement = currentReplacement.IsSome }
                match resolved.HasMarkReplacement, resolved.HasCurrentReplacement with
                | true, _ ->
                    struct (Emission.Emit (RunEdit.UnsafeToBreak (resolved.Mark, min (cursor.Index + 1) cursor.Input.Length)), { resolved with Phase = MarkReplace })
                | false, true ->
                    let struct (emission, next) = replacementEdit applyContext &cursor resolved resolved.CurrentTarget resolved.CurrentReplacement
                    struct (emission, { next with Phase = Done })
                | false, false -> struct (Emission.Complete, { resolved with MarkSet = resolved.NextMarkSet; Mark = resolved.NextMark })

/// <summary>Whether an entry of a contextual subtable names a substitution, <c>ContextualSubtable::is_actionable</c>.</summary>
[<RequireQualifiedAccess>]
module internal MorxContextualEntry =
    /// <summary>Whether an entry names a mark or a current substitution table.</summary>
    /// <param name="extended">Whether the table is morx, whose absent index is 0xFFFF.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>True when either index names a table.</returns>
    let isActionable (extended: bool) (entry: Entry) =
        match extended with
        | true -> entry.Data0 <> 0xFFFFus || entry.Data1 <> 0xFFFFus
        | false -> entry.Data0 <> 0us || entry.Data1 <> 0us

/// <summary>The contextual morx/mort subtable kind (src/hb-aat-layout-morx-table.hh:197-382).</summary>
[<Struct; NoComparison; NoEquality>]
type internal ContextualKind =
    { Extended: bool
      Body: ReadOnlyMemory<byte>
      SubstitutionTablesAt: int
      NumGlyphs: int
      Context: AatContext }

    interface IStateMachineKind<ContextualContext> with
        member _.InPlace = true
        member this.IsActionable entry = MorxContextualEntry.isActionable this.Extended entry
        member this.IsActionInitiable entry = entry.Flags &&& 0x8000us <> 0us
        member this.Transition (cursor, entry, context) =
            MorxContextual.transition this.Extended this.Body this.SubstitutionTablesAt this.NumGlyphs this.Context &cursor entry context
        member this.NullTransition (cursor, entry, context) =
            match entry.Flags &&& 0x8000us <> 0us with
            | true -> { context with MarkSet = true; Mark = cursor.Index }
            | false -> context
