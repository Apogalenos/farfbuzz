namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// <summary>The normalizer of <c>hb-ot-shape-normalize.cc</c>: decompose, reorder and recompose.</summary>
[<RequireQualifiedAccess>]
module internal Normalize =
    /// <summary>The longest mark run that is reordered, <c>HB_OT_SHAPE_MAX_COMBINING_MARKS</c>.</summary>
    [<Literal>]
    let private MaxCombiningMarks = 32

    /// <summary>The combining grapheme joiner, <c>0x034F</c>.</summary>
    [<Literal>]
    let private Cgj = 0x034Fu

    /// <summary>The no-break hyphen, mapped to the hyphen's glyph, <c>0x2011</c>.</summary>
    [<Literal>]
    let private NoBreakHyphen = 0x2011u

    /// <summary>The hyphen whose glyph serves U+2011, <c>0x2010</c>.</summary>
    [<Literal>]
    let private Hyphen = 0x2010u

    /// <summary>The space code point, the source of a fallback space glyph, <c>0x0020</c>.</summary>
    [<Literal>]
    let private Space = 0x0020u

    /// <summary>The fraction slash, <c>0x2044</c>.</summary>
    [<Literal>]
    let private FractionSlash = 0x2044u

    /// <summary>The invalid code point, <c>HB_CODEPOINT_INVALID</c>.</summary>
    [<Literal>]
    let private CodepointInvalid = 0xFFFFFFFFu

    /// <summary>Looks up a nominal glyph with a custom not-found value, <c>get_nominal_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="notFound">The glyph returned when the code point has none.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph.</returns>
    let private nominalOr font notFound codepoint =
        match Font.nominalGlyph font codepoint with
        | ValueSome glyph -> glyph
        | ValueNone -> notFound

    /// <summary>Looks up a nominal glyph with zero as the miss, <c>set_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph, or zero.</returns>
    let private nominalZero font codepoint =
        nominalOr font 0u codepoint

    /// <summary>Builds the two edits of <c>next_char</c>: set the glyph index, then advance.</summary>
    /// <param name="props">The current glyph's properties, already carrying the glyph index.</param>
    /// <returns>The edits.</returns>
    let private nextCharEdits props =
        Sealed.ofSpan (ReadOnlySpan [| RunEdit.SetCurrentProps props; RunEdit.NextGlyph |])

    /// <summary>Builds <c>[a]</c> or <c>[a, b]</c>, the output of one decompose step.</summary>
    /// <param name="aGlyph">The glyph of <paramref name="a"/>.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="bPart">The optional second part.</param>
    /// <returns>The parts.</returns>
    let private pairWithB aGlyph a bPart =
        let first = Sealed.appendOne (ReadOnlySpan<struct (uint32 * uint32)>()) (struct (a, aGlyph))
        match bPart with
        | ValueNone -> first
        | ValueSome part -> Sealed.appendOne (first.AsSpan()) part

    /// <summary>Appends the optional second part after a sub-decomposition.</summary>
    /// <param name="sub">The sub-decomposition of the first part.</param>
    /// <param name="bPart">The optional second part.</param>
    /// <returns>The parts.</returns>
    let private appendB sub bPart =
        match bPart with
        | ValueNone -> sub
        | ValueSome part -> Sealed.appendOne (sub.AsSpan()) part

    /// <summary>Decomposes one code point, failing when the second part has no glyph, <c>decompose</c>'s first checks.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The first part and the optional second part, or an error.</returns>
    let private tryDecompose plan font u : Result<struct (uint32 * struct (uint32 * uint32) voption), unit> =
        match ShaperHooks.decompose plan font u with
        | ValueNone -> Error ()
        | ValueSome (struct (a, b)) ->
            match b = 0u with
            | true -> Ok (struct (a, ValueNone))
            | false ->
                match Font.nominalGlyph font b with
                | ValueNone -> Error ()
                | ValueSome glyph -> Ok (struct (a, ValueSome (struct (b, glyph))))

    /// <summary>Decomposes one code point into the parts the font supports, <c>decompose</c>. The mutual recursion
    /// with <see cref="finishDecompose"/> follows the bounded Unicode decomposition chain, at most four deep.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="shortest">Whether to stop at the first glyph.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The parts, empty when the code point does not decompose.</returns>
    let rec private decomposeParts plan font shortest u : ImmutableArray<struct (uint32 * uint32)> =
        match tryDecompose plan font u with
        | Error () -> ImmutableArray.Empty
        | Ok (struct (a, bPart)) ->
            let hasA = Font.nominalGlyph font a
            match shortest && hasA.IsSome with
            | true -> pairWithB hasA.Value a bPart
            | false -> finishDecompose plan font shortest hasA a bPart

    /// <summary>Resolves the sub-decomposition of <c>a</c>, falling back to <c>a</c> when it has a glyph, <c>decompose</c>'s tail.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="shortest">Whether to stop at the first glyph.</param>
    /// <param name="hasA">The glyph of the first part, when it has one.</param>
    /// <param name="a">The first part.</param>
    /// <param name="bPart">The optional second part.</param>
    /// <returns>The parts, empty when nothing decomposes.</returns>
    and private finishDecompose plan font shortest hasA a bPart : ImmutableArray<struct (uint32 * uint32)> =
        let sub = decomposeParts plan font shortest a
        match sub.IsEmpty with
        | true ->
            match hasA with
            | ValueSome glyph -> pairWithB glyph a bPart
            | ValueNone -> ImmutableArray.Empty
        | false -> appendB sub bPart

    /// <summary>Appends the second part of a decomposition to the parts written so far.</summary>
    /// <param name="parts">The array the parts are written to.</param>
    /// <param name="at">The index to write the part to.</param>
    /// <param name="b">The second code point, zero for none.</param>
    /// <param name="glyph">The glyph of the second code point.</param>
    /// <returns>The number of parts the second part adds.</returns>
    let private appendPart (parts: struct (uint32 * uint32) array) (at: int) (b: uint32) (glyph: uint32) =
        match b = 0u with
        | true -> 0
        | false ->
            Sealed.storePart parts at b glyph
            1

    /// <summary>Decomposes one code point into the parts the font supports as <see cref="decomposeParts"/> does, writing them into an
    /// array instead of building arrays.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="shortest">Whether to stop at the first glyph.</param>
    /// <param name="u">The code point.</param>
    /// <param name="parts">The array the parts are written to.</param>
    /// <param name="start">The index the first part is written to.</param>
    /// <returns>The number of parts, zero when the code point does not decompose.</returns>
    let rec private decomposeInto plan font shortest u (parts: struct (uint32 * uint32) array) (start: int) =
        match ShaperHooks.decompose plan font u with
        | ValueNone -> 0
        | ValueSome (struct (a, b)) ->
            match (match b = 0u with | true -> ValueSome 0u | false -> Font.nominalGlyph font b) with
            | ValueNone -> 0
            | ValueSome glyphB ->
                let hasA = Font.nominalGlyph font a
                let written =
                    match shortest && hasA.IsSome with
                    | true -> 0
                    | false -> decomposeInto plan font shortest a parts start
                match written, hasA with
                | 0, ValueSome glyphA ->
                    Sealed.storePart parts start a glyphA
                    1 + appendPart parts (start + 1) b glyphB
                | 0, ValueNone -> 0
                | count, _ -> count + appendPart parts (start + count) b glyphB

    /// <summary>The packed Unicode properties of a code point the normalizer produces, <c>_hb_glyph_info_set_unicode_props</c> through the
    /// Unicode functions of the plan.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The packed properties.</returns>
    let private propsOf (plan: OtShapePlan) codepoint =
        match plan.Unicode with
        | ValueNone ->
            let struct (props, _) = UnicodeProps.compute codepoint
            props
        | ValueSome unicode ->
            let struct (props, _) = UnicodeProps.computeWith unicode codepoint
            props

    /// <summary>Builds the output edits of one decomposed character, <c>output_char</c> per part then <c>skip_char</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="baseProps">The current glyph's properties.</param>
    /// <param name="parts">The decomposition parts.</param>
    /// <returns>The edits.</returns>
    let private decomposeCharEdits plan info baseProps (parts: ImmutableArray<struct (uint32 * uint32)>) =
        let edits =
            Sealed.init parts.Length (fun index ->
                let struct (codepoint, glyph) = parts[index]
                let props = { baseProps with GlyphIndex = glyph; UnicodeProps = propsOf plan codepoint }
                RunEdit.OutputInfo ({ info with Codepoint = codepoint }, props))
        Sealed.appendOne (edits.AsSpan()) RunEdit.SkipGlyph

    /// <summary>Handles the no-break hyphen and the final not-found glyph of <c>decompose_current_character</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="notFound">The not-found glyph.</param>
    /// <param name="props">The current glyph's properties.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The edits.</returns>
    let private hyphenOrNotFound font notFound props u =
        match u = NoBreakHyphen with
        | true ->
            match Font.nominalGlyph font Hyphen with
            | ValueSome glyph -> nextCharEdits { props with GlyphIndex = glyph }
            | ValueNone -> nextCharEdits { props with GlyphIndex = notFound }
        | false -> nextCharEdits { props with GlyphIndex = notFound }

    /// <summary>Maps a space without a glyph to the space glyph or the invisible glyph, <c>decompose_current_character</c>'s space branch.</summary>
    /// <param name="font">The font.</param>
    /// <param name="notFound">The not-found glyph.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="props">The current glyph's properties.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The edits.</returns>
    let private spaceOrFallback font notFound invisible props u =
        match UnicodeProps.isUnicodeSpace props with
        | false -> hyphenOrNotFound font notFound props u
        | true ->
            let spaceType = Unicode.spaceFallbackType u
            let spaceGlyph = Font.nominalGlyph font Space
            match struct (spaceType, spaceGlyph, invisible) with
            | struct (SpaceFallback.NotSpace, _, _)
            | struct (_, ValueNone, 0u) -> hyphenOrNotFound font notFound props u
            | struct (_, ValueSome glyph, _)
            | struct (_, ValueNone, glyph) ->
                nextCharEdits { (UnicodeProps.setSpaceFallback spaceType props) with GlyphIndex = glyph }

    /// <summary>Resolves the non-short-circuit nominal lookup and the space fallback, <c>decompose_current_character</c>'s tail.</summary>
    /// <param name="font">The font.</param>
    /// <param name="shortest">Whether the short-circuit lookup already missed.</param>
    /// <param name="notFound">The not-found glyph.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="props">The current glyph's properties.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The edits.</returns>
    let private fallbackEdits font shortest notFound invisible props u =
        let nominalGlyph =
            match shortest with
            | false -> Font.nominalGlyph font u
            | true -> ValueNone
        match nominalGlyph with
        | ValueSome glyph -> nextCharEdits { props with GlyphIndex = glyph }
        | ValueNone -> spaceOrFallback font notFound invisible props u

    /// <summary>Computes the edits of <c>decompose_current_character</c> for one glyph.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="shortest">Whether to short-circuit on the first glyph.</param>
    /// <param name="notFound">The not-found glyph.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph's properties.</param>
    /// <returns>The edits.</returns>
    let private decomposeCurrentEdits plan font shortest notFound invisible info props =
        let directGlyph =
            match shortest with
            | true -> Font.nominalGlyph font info.Codepoint
            | false -> ValueNone
        match directGlyph with
        | ValueSome glyph -> nextCharEdits { props with GlyphIndex = glyph }
        | ValueNone ->
            let parts = decomposeParts plan font shortest info.Codepoint
            match parts.IsEmpty with
            | false -> decomposeCharEdits plan info props parts
            | true -> fallbackEdits font shortest notFound invisible props info.Codepoint

    /// <summary>Builds the <c>set_glyph</c> then <c>next_glyph</c> edits of the variation-selector cluster.</summary>
    /// <param name="font">The font.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The edits.</returns>
    let private vsSetGlyphNext font info props =
        nextCharEdits { props with GlyphIndex = nominalZero font info.Codepoint }

    /// <summary>Builds the edits of one variation-selector pair, <c>handle_variation_selector_cluster</c>'s hit and miss branches.</summary>
    /// <param name="font">The font.</param>
    /// <param name="notFoundVariationSelector">The variation-selector not-found glyph.</param>
    /// <param name="baseInfo">The base glyph.</param>
    /// <param name="baseProps">The base glyph's properties.</param>
    /// <param name="selectorInfo">The selector glyph.</param>
    /// <param name="selectorProps">The selector glyph's properties.</param>
    /// <returns>The edits.</returns>
    let private vsPairEdits font notFoundVariationSelector baseInfo baseProps selectorInfo selectorProps =
        match Font.variationGlyph font baseInfo.Codepoint selectorInfo.Codepoint with
        | ValueSome glyph ->
            Sealed.ofSpan (ReadOnlySpan [|
                RunEdit.SetCurrentProps { baseProps with GlyphIndex = glyph }
                RunEdit.ReplaceGlyphs (2, Sealed.ofSpan (ReadOnlySpan [| baseInfo.Codepoint |]))
            |])
        | ValueNone ->
            let baseGlyph = nominalZero font baseInfo.Codepoint
            let selectorProps = UnicodeProps.setVariationSelector true selectorProps
            let selectorProps =
                match notFoundVariationSelector = CodepointInvalid with
                | true -> selectorProps
                | false -> UnicodeProps.clearDefaultIgnorable selectorProps
            let selectorProps = { selectorProps with GlyphIndex = nominalZero font selectorInfo.Codepoint }
            Sealed.ofSpan (ReadOnlySpan [|
                RunEdit.SetCurrentProps { baseProps with GlyphIndex = baseGlyph }
                RunEdit.NextGlyph
                RunEdit.SetCurrentProps selectorProps
                RunEdit.NextGlyph
            |])

    /// <summary>The state of the decompose round.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DecomposeState =
        | Open
        | NextChar of boundary: int * multiCluster: bool
        | SimpleChar of boundary: int * multiCluster: bool
        | Pending of boundary: int * multiCluster: bool * edits: ImmutableArray<RunEdit> * editIndex: int
        | MarksScan of boundary: int
        | VsCluster of boundary: int
        | VsSkip of boundary: int
        | VsPending of boundary: int * resume: int * edits: ImmutableArray<RunEdit> * editIndex: int
        | Done

    /// <summary>Finds the end of a simple run: one before the first mark, <c>hb-ot-shape-normalize.cc:333-338</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="endIndex">The candidate end.</param>
    /// <returns>The boundary.</returns>
    [<TailCall>]
    let rec private simpleBoundaryScan (props: ReadOnlySpan<GlyphProps>) endIndex =
        match endIndex >= props.Length with
        | true -> endIndex
        | false ->
            match UnicodeProps.isUnicodeMark props[endIndex] with
            | true -> endIndex - 1
            | false -> simpleBoundaryScan props (endIndex + 1)

    /// <summary>Finds the end of a simple run from <paramref name="index"/>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The run start.</param>
    /// <returns>The boundary.</returns>
    let private simpleBoundary (props: ReadOnlySpan<GlyphProps>) index =
        simpleBoundaryScan props (index + 1)

    /// <summary>Finds the end of a mark run: the first non-mark, <c>hb-ot-shape-normalize.cc:359-361</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="endIndex">The candidate end.</param>
    /// <returns>The boundary.</returns>
    [<TailCall>]
    let rec private marksBoundaryScan (props: ReadOnlySpan<GlyphProps>) endIndex =
        match endIndex >= props.Length with
        | true -> endIndex
        | false ->
            match UnicodeProps.isUnicodeMark props[endIndex] with
            | false -> endIndex
            | true -> marksBoundaryScan props (endIndex + 1)

    /// <summary>Finds the end of a mark run from <paramref name="index"/>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The run start.</param>
    /// <returns>The boundary.</returns>
    let private marksBoundary (props: ReadOnlySpan<GlyphProps>) index =
        marksBoundaryScan props (index + 1)

    /// <summary>Finds a variation selector in a range, <c>decompose_multi_char_cluster</c>'s scan.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="index">The scan position.</param>
    /// <param name="boundary">The range end.</param>
    /// <returns>True when a variation selector is present.</returns>
    [<TailCall>]
    let rec private findVs (infos: ReadOnlySpan<GlyphInfo>) index boundary =
        match index >= boundary with
        | true -> false
        | false ->
            match Unicode.isVariationSelector infos[index].Codepoint with
            | true -> true
            | false -> findVs infos (index + 1) boundary

    /// <summary>Chooses the state after a run finishes: the next run, the mark scan or done. A run that has failed is
    /// done, as the round's loop condition <c>buffer->successful</c> ends it at once.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The cursor.</param>
    /// <param name="successful">Whether the run has not failed.</param>
    /// <param name="multiCluster">Whether the finished run was a mark cluster.</param>
    /// <returns>The next state.</returns>
    let private afterRunState (props: ReadOnlySpan<GlyphProps>) index successful multiCluster =
        match index >= props.Length || not successful with
        | true -> Done
        | false ->
            match multiCluster with
            | false -> MarksScan (marksBoundary props index)
            | true -> NextChar (simpleBoundary props index, false)

    /// <summary>Whether a missed glyph is plain: no decomposition, not a space, not the no-break hyphen.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <param name="u">The code point.</param>
    /// <returns>True when the miss maps straight to the not-found glyph.</returns>
    let private plainNotFound (plan: OtShapePlan) (font: Font) (props: GlyphProps) (u: uint32) =
        match tryDecompose plan font u with
        | Ok _ -> false
        | Error () ->
            match UnicodeProps.isUnicodeSpace props with
            | true -> false
            | false -> u <> NoBreakHyphen

    /// <summary>Produces the edits of one glyph, taking the short-circuit fast path when the glyph or a plain miss is known.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="notFound">The not-found glyph.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="boundary">The run boundary.</param>
    /// <param name="multiCluster">Whether the run is a mark cluster.</param>
    /// <param name="shortest">Whether to short-circuit on the first glyph.</param>
    /// <returns>The edit and the next state.</returns>
    let private nextCharStep (plan: OtShapePlan) (font: Font) (notFound: uint32) (invisible: uint32) (cursor: RunCursor) (boundary: int) (multiCluster: bool) (shortest: bool) =
        match shortest with
        | true ->
            let info = cursor.Input[cursor.Index]
            let props = cursor.InputProps[cursor.Index]
            match Font.nominalGlyph font info.Codepoint with
            | ValueSome glyph ->
                struct (RunEdit.SetCurrentProps { props with GlyphIndex = glyph }, SimpleChar (boundary, multiCluster))
            | ValueNone ->
                match plainNotFound plan font props info.Codepoint with
                | true -> struct (RunEdit.SetCurrentProps { props with GlyphIndex = notFound }, SimpleChar (boundary, multiCluster))
                | false ->
                    let edits = decomposeCurrentEdits plan font true notFound invisible info props
                    struct (edits[0], Pending (boundary, multiCluster, edits, 1))
        | false ->
            let edits = decomposeCurrentEdits plan font false notFound invisible cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
            struct (edits[0], Pending (boundary, multiCluster, edits, 1))

    /// <summary>The decompose round as an out-buffer pass.</summary>
    [<Struct>]
    type private DecomposePass =
        val Plan: OtShapePlan
        val Font: Font
        val NotFound: uint32
        val Invisible: uint32
        val NotFoundVariationSelector: uint32
        val MightShortCircuit: bool
        val AlwaysShortCircuit: bool

        /// <summary>Creates a decompose pass.</summary>
        /// <param name="plan">The plan.</param>
        /// <param name="font">The font.</param>
        /// <param name="notFound">The not-found glyph.</param>
        /// <param name="invisible">The invisible glyph.</param>
        /// <param name="notFoundVariationSelector">The variation-selector not-found glyph.</param>
        /// <param name="mightShortCircuit">The short-circuit flag for simple runs.</param>
        /// <param name="alwaysShortCircuit">The short-circuit flag for mark clusters.</param>
        new (plan, font, notFound, invisible, notFoundVariationSelector, mightShortCircuit, alwaysShortCircuit) =
            { Plan = plan
              Font = font
              NotFound = notFound
              Invisible = invisible
              NotFoundVariationSelector = notFoundVariationSelector
              MightShortCircuit = mightShortCircuit
              AlwaysShortCircuit = alwaysShortCircuit }

        interface IRunPass<DecomposeState> with
            member this.Step (cursor, state) =
                match state with
                | Open ->
                    struct (RunEdit.ClearOutput, NextChar (simpleBoundary cursor.InputProps 0, false))
                | NextChar (boundary, multiCluster) ->
                    match cursor.Index < boundary with
                    | false -> struct (RunEdit.NextGlyphs 0, afterRunState cursor.InputProps cursor.Index cursor.Successful multiCluster)
                    | true when not cursor.Successful -> struct (RunEdit.Finish, Done)
                    | true ->
                        let shortest = if multiCluster then this.AlwaysShortCircuit else this.MightShortCircuit
                        nextCharStep this.Plan this.Font this.NotFound this.Invisible cursor boundary multiCluster shortest
                | SimpleChar (boundary, multiCluster) ->
                    struct (RunEdit.NextGlyph, NextChar (boundary, multiCluster))
                | Pending (boundary, multiCluster, edits, editIndex) ->
                    match editIndex < edits.Length with
                    | true -> struct (edits[editIndex], Pending (boundary, multiCluster, edits, editIndex + 1))
                    | false -> struct (RunEdit.NextGlyphs 0, NextChar (boundary, multiCluster))
                | MarksScan boundary ->
                    match cursor.Successful && findVs cursor.Input cursor.Index boundary with
                    | false -> struct (RunEdit.NextGlyphs 0, NextChar (boundary, true))
                    | true -> struct (RunEdit.NextGlyphs 0, VsCluster boundary)
                | VsCluster boundary ->
                    match cursor.Index < boundary - 1 && cursor.Successful with
                    | false ->
                        match cursor.Index < boundary with
                        | false -> struct (RunEdit.NextGlyphs 0, afterRunState cursor.InputProps cursor.Index cursor.Successful true)
                        | true ->
                            let edits = vsSetGlyphNext this.Font cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
                            struct (edits[0], VsPending (boundary, 2, edits, 1))
                    | true ->
                        match Unicode.isVariationSelector cursor.Input[cursor.Index + 1].Codepoint with
                        | false ->
                            let edits = vsSetGlyphNext this.Font cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
                            struct (edits[0], VsPending (boundary, 0, edits, 1))
                        | true ->
                            let edits = vsPairEdits this.Font this.NotFoundVariationSelector cursor.Input[cursor.Index] cursor.InputProps[cursor.Index] cursor.Input[cursor.Index + 1] cursor.InputProps[cursor.Index + 1]
                            struct (edits[0], VsPending (boundary, 1, edits, 1))
                | VsSkip boundary ->
                    match cursor.Index < boundary && cursor.Successful && Unicode.isVariationSelector cursor.Input[cursor.Index].Codepoint with
                    | true ->
                        let edits = vsSetGlyphNext this.Font cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
                        struct (edits[0], VsPending (boundary, 1, edits, 1))
                    | false -> struct (RunEdit.NextGlyphs 0, VsCluster boundary)
                | VsPending (boundary, resume, edits, editIndex) ->
                    match editIndex < edits.Length with
                    | true -> struct (edits[editIndex], VsPending (boundary, resume, edits, editIndex + 1))
                    | false ->
                        match resume with
                        | 0 -> struct (RunEdit.NextGlyphs 0, VsCluster boundary)
                        | 1 -> struct (RunEdit.NextGlyphs 0, VsSkip boundary)
                        | _ -> struct (RunEdit.NextGlyphs 0, afterRunState cursor.InputProps cursor.Index cursor.Successful true)
                | Done -> struct (RunEdit.Finish, Done)

    /// <summary>Runs the decompose round.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="alwaysShortCircuit">The short-circuit flag for mark clusters.</param>
    /// <param name="mightShortCircuit">The short-circuit flag for simple runs.</param>
    /// <param name="run">The run.</param>
    /// <returns>The decomposed run.</returns>
    let internal decomposeRound plan font alwaysShortCircuit mightShortCircuit run =
        GlyphRun.runPass (DecomposePass(plan, font, run.NotFound, run.Invisible, run.NotFoundVariationSelector, mightShortCircuit, alwaysShortCircuit)) Open run

    /// <summary>Runs the decompose round for a simple run: a batch nominal lookup maps every glyph, and any miss, or a run that has already
    /// failed (the round's loop runs only while the buffer is successful, so it maps nothing), falls back to the out-buffer machine,
    /// <c>get_nominal_glyphs</c> (src/hb-ot-shape-normalize.cc:333-355).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="alwaysShortCircuit">The short-circuit flag for mark clusters.</param>
    /// <param name="mightShortCircuit">The short-circuit flag for simple runs.</param>
    /// <param name="run">The simple run.</param>
    /// <returns>The decomposed run.</returns>
    let internal decomposeSimple plan font alwaysShortCircuit mightShortCircuit run =
        match run.Successful with
        | false -> decomposeRound plan font alwaysShortCircuit mightShortCircuit run
        | true ->
            match Font.nominalGlyphsAll font (run.Infos.AsSpan()) with
            | ValueSome glyphs -> { run with Props = Sealed.mapi (fun index (props: GlyphProps) -> { props with GlyphIndex = glyphs[index] }) (run.Props.AsSpan()); HavePositions = false; Positions = ImmutableArray.Empty }
            | ValueNone -> decomposeRound plan font alwaysShortCircuit mightShortCircuit run

    /// <summary>Whether any mark sits after the first glyph, the inverse of the decompose round's <c>all_simple</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The scan position.</param>
    /// <returns>True when a mark follows the first glyph.</returns>
    [<TailCall>]
    let rec private hasMarkFrom (props: ReadOnlySpan<GlyphProps>) index =
        match index >= props.Length with
        | true -> false
        | false ->
            match UnicodeProps.isUnicodeMark props[index] with
            | true -> true
            | false -> hasMarkFrom props (index + 1)

    /// <summary>Compares two glyphs by their modified combining class, <c>compare_combining_class</c>.</summary>
    [<Struct>]
    type private CccComparer =
        interface IGlyphPropsComparer with
            member _.Compare (_, aProps, _, bProps) =
                compare (UnicodeProps.modifiedCombiningClass aProps) (UnicodeProps.modifiedCombiningClass bProps)

    /// <summary>Finds the end of a run of non-zero combining classes, <c>hb-ot-shape-normalize.cc:384-386</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The scan position.</param>
    /// <returns>The first zero-class index.</returns>
    [<TailCall>]
    let rec private cccRunEnd (props: ReadOnlySpan<GlyphProps>) index =
        match index >= props.Length with
        | true -> index
        | false ->
            match UnicodeProps.modifiedCombiningClass props[index] = 0uy with
            | true -> index
            | false -> cccRunEnd props (index + 1)

    /// <summary>Reorders mark runs by combining class, then by the shaper's <c>reorder_marks</c> hook,
    /// <c>hb-ot-shape-normalize.cc:372-401</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The reordered run.</returns>
    [<TailCall>]
    let rec private reorderFrom plan run index =
        match index >= run.Infos.Length with
        | true -> run
        | false ->
            match UnicodeProps.modifiedCombiningClass run.Props[index] = 0uy with
            | true -> reorderFrom plan run (index + 1)
            | false ->
                let finish = cccRunEnd (run.Props.AsSpan()) (index + 1)
                match finish - index > MaxCombiningMarks with
                | true -> reorderFrom plan run (finish + 1)
                | false -> reorderFrom plan (ShaperHooks.reorderMarks plan (GlyphRun.sortClustersByProps index finish (CccComparer()) run) index finish) (finish + 1)

    /// <summary>Unhides a combining grapheme joiner that prevented no reordering, <c>hb-ot-shape-normalize.cc:403-417</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with any passive joiner unhidden.</returns>
    let private cgjUnhide run =
        match (run.ScratchFlags &&& ScratchFlags.HasCgj) = ScratchFlags.Default with
        | true -> run
        | false ->
            let infos = run.Infos
            let props = run.Props
            let newProps =
                Sealed.init props.Length (fun index ->
                    match index > 0 && index + 1 < props.Length && infos[index].Codepoint = Cgj with
                    | false -> props[index]
                    | true ->
                        let nextCc = UnicodeProps.modifiedCombiningClass props[index + 1]
                        let prevCc = UnicodeProps.modifiedCombiningClass props[index - 1]
                        match nextCc = 0uy || prevCc <= nextCc with
                        | true -> UnicodeProps.unhide props[index]
                        | false -> props[index])
            { run with Props = newProps }

    /// <summary>The state of the compose round.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ComposeState =
        | ComposeOpen
        | ComposeFirst
        | ComposeLoop of starter: int
        | ComposePending of starter: int * edits: ImmutableArray<RunEdit> * editIndex: int
        | ComposeDone

    /// <summary>Chooses the starter after a blocked glyph, <c>hb-ot-shape-normalize.cc:472-473</c>.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <param name="starter">The current starter.</param>
    /// <param name="outLen">The output length before the advance.</param>
    /// <returns>The next starter.</returns>
    let private blockStarter props starter outLen =
        match UnicodeProps.modifiedCombiningClass props = 0uy with
        | true -> outLen
        | false -> starter

    /// <summary>Decides whether a mark composes with its starter, <c>hb-ot-shape-normalize.cc:436-476</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="starter">The starter index.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="starterInfo">The starter glyph.</param>
    /// <param name="starterProps">The starter properties.</param>
    /// <param name="prevProps">The previous output properties.</param>
    /// <param name="cur">The current glyph.</param>
    /// <param name="curProps">The current properties.</param>
    /// <returns>The edit and next state.</returns>
    let private composeMarkStep plan font starter outLen starterInfo starterProps prevProps cur curProps =
        let ccCur = UnicodeProps.modifiedCombiningClass curProps
        let prevCc = UnicodeProps.modifiedCombiningClass prevProps
        let unblocked = starter = outLen - 1 || prevCc < ccCur
        match unblocked with
        | false -> struct (RunEdit.NextGlyph, ComposeLoop (blockStarter curProps starter outLen))
        | true ->
            match ShaperHooks.compose plan starterInfo.Codepoint cur.Codepoint with
            | ValueNone -> struct (RunEdit.NextGlyph, ComposeLoop (blockStarter curProps starter outLen))
            | ValueSome composed ->
                match Font.nominalGlyph font composed with
                | ValueNone -> struct (RunEdit.NextGlyph, ComposeLoop (blockStarter curProps starter outLen))
                | ValueSome glyph ->
                    let newProps = { starterProps with GlyphIndex = glyph; UnicodeProps = propsOf plan composed }
                    let edits =
                        Sealed.ofSpan (ReadOnlySpan [|
                            RunEdit.NextGlyph
                            RunEdit.MergeOutClusters (starter, outLen + 1)
                            RunEdit.PopOutput
                            RunEdit.SetOutputGlyph (starter, composed, newProps)
                        |])
                    struct (edits[0], ComposePending (starter, edits, 1))

    /// <summary>The compose round as an out-buffer pass.</summary>
    [<Struct>]
    type private ComposePass =
        val Plan: OtShapePlan
        val Font: Font

        /// <summary>Creates a compose pass.</summary>
        /// <param name="plan">The plan.</param>
        /// <param name="font">The font.</param>
        new (plan, font) = { Plan = plan; Font = font }

        interface IRunPass<ComposeState> with
            member this.Step (cursor, state) =
                match state with
                | ComposeOpen -> struct (RunEdit.ClearOutput, ComposeFirst)
                | ComposeFirst -> struct (RunEdit.NextGlyph, ComposeLoop 0)
                | ComposeLoop starter ->
                    match cursor.Index >= cursor.Input.Length with
                    | true -> struct (RunEdit.Finish, ComposeDone)
                    | false ->
                        let curProps = cursor.InputProps[cursor.Index]
                        match UnicodeProps.isUnicodeMark curProps with
                        | false ->
                            let next = blockStarter curProps starter cursor.Output.Length
                            struct (RunEdit.NextGlyph, ComposeLoop next)
                        | true ->
                            composeMarkStep this.Plan this.Font starter cursor.Output.Length cursor.Output[starter] cursor.OutputProps[starter] cursor.OutputProps[cursor.Output.Length - 1] cursor.Input[cursor.Index] curProps
                | ComposePending (starter, edits, editIndex) ->
                    match editIndex + 1 < edits.Length with
                    | true -> struct (edits[editIndex], ComposePending (starter, edits, editIndex + 1))
                    | false -> struct (edits[editIndex], ComposeLoop starter)
                | ComposeDone -> struct (RunEdit.Finish, ComposeDone)

    /// <summary>Runs the compose round.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The recomposed run.</returns>
    let private composeRound plan font run =
        GlyphRun.runPass (ComposePass(plan, font)) ComposeOpen run

    /// <summary>Accumulates the scratch flags a normalized output carries.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The properties.</param>
    /// <param name="index">The next index.</param>
    /// <param name="scratch">The flags so far.</param>
    /// <returns>The flags.</returns>
    [<TailCall>]
    let rec internal scanScratchFrom (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index scratch =
        match index < infos.Length with
        | false -> scratch
        | true ->
            let scratch =
                match UnicodeProps.isContinuation props[index] with
                | true -> scratch ||| ScratchFlags.HasContinuations
                | false -> scratch
            let scratch =
                match props[index].UnicodeProps &&& UnicodeProps.Ignorable <> 0us with
                | true -> scratch ||| ScratchFlags.HasDefaultIgnorables
                | false -> scratch
            let scratch =
                match infos[index].Codepoint with
                | Cgj -> scratch ||| ScratchFlags.HasCgj
                | FractionSlash -> scratch ||| ScratchFlags.HasFractionSlash
                | _ -> scratch
            let scratch =
                match UnicodeProps.spaceFallback props[index] = SpaceFallback.NotSpace with
                | true -> scratch
                | false -> scratch ||| ScratchFlags.HasSpaceFallback
            let scratch =
                match UnicodeProps.isVariationSelector props[index] with
                | true -> scratch ||| ScratchFlags.HasVariationSelectorFallback
                | false -> scratch
            scanScratchFrom infos props (index + 1) scratch

    /// <summary>The packed general-category value of the format category, the one whose glyphs may be variation selectors.</summary>
    [<Literal>]
    let private FormatCategoryValue = 1us

    /// <summary>The packed general-category value of the space separators, the category whose glyphs may be space fallbacks.</summary>
    [<Literal>]
    let private SpaceSeparatorCategoryValue = 29us

    /// <summary>Accumulates the scratch flags a normalized output carries as <see cref="scanScratchFrom"/> does, reading the five tests of a
    /// glyph only when one of them can hold: a glyph that is not default-ignorable, not of the format or space-separator category and neither
    /// the combining grapheme joiner nor the fraction slash adds the continuation flag when it is a continuation and none other, whichever
    /// the test.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The properties.</param>
    /// <param name="index">The next index.</param>
    /// <param name="scratch">The flags so far.</param>
    /// <returns>The flags.</returns>
    [<TailCall>]
    let rec internal scanScratchQuick (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index scratch =
        match index < infos.Length with
        | false -> scratch
        | true ->
            let value = props[index].UnicodeProps
            let category = value &&& UnicodeProps.GenCatMask
            let plain = value &&& UnicodeProps.Ignorable = 0us && category <> FormatCategoryValue && category <> SpaceSeparatorCategoryValue && infos[index].Codepoint <> Cgj && infos[index].Codepoint <> FractionSlash
            match plain, value &&& UnicodeProps.Continuation = 0us with
            | true, true -> scanScratchQuick infos props (index + 1) scratch
            | true, false -> scanScratchQuick infos props (index + 1) (scratch ||| ScratchFlags.HasContinuations)
            | false, _ -> scanScratchQuick infos props (index + 1) (scanScratchFrom (infos.Slice(index, 1)) (props.Slice(index, 1)) 0 scratch)

    /// <summary>The scratch flag a test adds.</summary>
    /// <param name="test">Whether the glyph has the property.</param>
    /// <param name="flag">The flag.</param>
    /// <returns>The flag, or none.</returns>
    let private flagWhen (test: bool) (flag: ScratchFlags) =
        match test with
        | true -> flag
        | false -> ScratchFlags.Default

    /// <summary>The scratch flags of a glyph that is not plain: a default ignorable, a format or space character, a combining grapheme
    /// joiner or a fraction slash, with the continuation and the variation selector tests, <see cref="scratchOfCode"/>'s second branch.</summary>
    /// <param name="codepoint">The glyph's code point.</param>
    /// <param name="unicode">The glyph's packed Unicode properties.</param>
    /// <returns>The flags.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private scratchOfUnusual (codepoint: uint32) (unicode: uint16) =
        let props = { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
        flagWhen (UnicodeProps.isContinuation props) ScratchFlags.HasContinuations
        ||| flagWhen (unicode &&& UnicodeProps.Ignorable <> 0us) ScratchFlags.HasDefaultIgnorables
        ||| flagWhen (codepoint = Cgj) ScratchFlags.HasCgj
        ||| flagWhen (codepoint = FractionSlash) ScratchFlags.HasFractionSlash
        ||| flagWhen (UnicodeProps.spaceFallback props <> SpaceFallback.NotSpace) ScratchFlags.HasSpaceFallback
        ||| flagWhen (UnicodeProps.isVariationSelector props) ScratchFlags.HasVariationSelectorFallback

    /// <summary>The scratch flags one glyph adds, the five tests of <see cref="scanScratchFrom"/> for one glyph, reading them only when
    /// one can hold as <see cref="scanScratchQuick"/> does: the plain glyph is answered inline, in a loop that asks it of every glyph.</summary>
    /// <param name="codepoint">The glyph's code point.</param>
    /// <param name="unicode">The glyph's packed Unicode properties.</param>
    /// <returns>The flags.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private scratchOfCode (codepoint: uint32) (unicode: uint16) =
        let category = unicode &&& UnicodeProps.GenCatMask
        let plain = unicode &&& UnicodeProps.Ignorable = 0us && category <> FormatCategoryValue && category <> SpaceSeparatorCategoryValue && codepoint <> Cgj && codepoint <> FractionSlash
        match plain with
        | true -> flagWhen (unicode &&& UnicodeProps.Continuation <> 0us) ScratchFlags.HasContinuations
        | false -> scratchOfUnusual codepoint unicode

    /// <summary>The scratch flags one glyph adds, <see cref="scratchOfCode"/> of its code point and packed properties.</summary>
    /// <param name="info">The glyph, whose code point is read.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The flags.</returns>
    let internal scratchOfGlyph (info: GlyphInfo) (props: GlyphProps) =
        scratchOfCode info.Codepoint props.UnicodeProps

    /// <summary>Returns the normalization mode a plan requests, <c>hb-ot-shape-normalize.cc:288-297</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>The mode.</returns>
    let mode plan =
        match plan.Shaper.Normalization with
        | NormalizationAuto -> NormalizationComposedDiacritics
        | set -> set

    /// <summary>Runs the reorder, unhide, compose and scratch rounds after the decompose round.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="allSimple">Whether every cluster was simple.</param>
    /// <param name="normalization">The normalization mode.</param>
    /// <param name="decomposed">The decomposed run.</param>
    /// <returns>The normalized run.</returns>
    let private finishRounds plan font allSimple (normalization: NormalizationMode) decomposed =
        let reordered =
            match allSimple || not decomposed.Successful with
            | true -> decomposed
            | false -> reorderFrom plan decomposed 0
        let cgj = cgjUnhide reordered
        let composed =
            match allSimple || not cgj.Successful || (normalization <> NormalizationComposedDiacritics && normalization <> NormalizationComposedDiacriticsNoShortCircuit) with
            | true -> cgj
            | false -> composeRound plan font cgj
        let scratch = scanScratchFrom (composed.Infos.AsSpan()) (composed.Props.AsSpan()) 0 ScratchFlags.Default
        { composed with ScratchFlags = composed.ScratchFlags ||| scratch }

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> entirely through the out-buffer machine,
    /// the reference the simple-run batch lookup must match.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The normalized run.</returns>
    let internal runPass plan font run =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let normalization = mode plan
            let alwaysShortCircuit = normalization = NormalizationNone
            let mightShortCircuit =
                alwaysShortCircuit
                || (normalization <> NormalizationDecomposed && normalization <> NormalizationComposedDiacriticsNoShortCircuit)
            let allSimple = not (hasMarkFrom (run.Props.AsSpan()) 1)
            finishRounds plan font allSimple normalization (decomposeRound plan font alwaysShortCircuit mightShortCircuit run)

    /// <summary>Whether a run holds a variation selector, which the lean normaliser leaves to the out-buffer passes.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when a variation selector is present.</returns>
    [<TailCall>]
    let rec private hasVariationSelector (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> false
        | false ->
            match Unicode.isVariationSelector infos[index].Codepoint with
            | true -> true
            | false -> hasVariationSelector infos (index + 1)

    /// <summary>The first code point with a canonical decomposition, <c>U+00C0</c>: the shapers the lean normaliser reads decompose through
    /// Unicode's table, which has no entry below it.</summary>
    [<Literal>]
    let private FirstDecomposable = 0xC0u

    /// <summary>The first Hebrew point, the first code point a Hebrew presentation form is composed with.</summary>
    [<Literal>]
    let private HebrewPointFirst = 0x05B0u

    /// <summary>The last Hebrew mark that composes into a presentation form.</summary>
    [<Literal>]
    let private HebrewPointLast = 0x05C7u

    /// <summary>The Bengali nukta, which the Indic compose hook composes with the letter ya although Unicode does not.</summary>
    [<Literal>]
    let private BengaliNukta = 0x09BCu

    /// <summary>The decisions of the lean normaliser: what the font maps, what a code point decomposes and composes into, and what the
    /// shaper's hooks say, each read from the same functions the out-buffer passes read.</summary>
    [<Struct>]
    type private LeanDecisions =
        val Plan: OtShapePlan
        val Font: Font
        val Cmap: CmapAccel
        val NotFoundGlyph: uint32
        val Invisible: uint32

        /// <summary>Creates the decisions.</summary>
        /// <param name="plan">The plan.</param>
        /// <param name="font">The font.</param>
        /// <param name="notFound">The not-found glyph.</param>
        /// <param name="invisible">The invisible glyph.</param>
        new (plan, font, notFound, invisible) = { Plan = plan; Font = font; Cmap = font.Ot.Cmap.Value; NotFoundGlyph = notFound; Invisible = invisible }

        interface INormalizeDecisions with
            member this.Glyph codepoint = Cmap.nominalGlyph this.Cmap codepoint
            member this.Page block = Cmap.pageOf this.Cmap block
            member this.PlainNotFound (props, codepoint) = plainNotFound this.Plan this.Font props codepoint
            member this.Undecomposable codepoint =
                match codepoint < FirstDecomposable with
                | true -> true
                | false ->
                    match ShaperHooks.decompose this.Plan this.Font codepoint with
                    | ValueNone -> true
                    | ValueSome _ -> false
            member this.NotFound = this.NotFoundGlyph
            member this.Edits (info, props, shortest) = decomposeCurrentEdits this.Plan this.Font shortest this.NotFoundGlyph this.Invisible info props
            member _.IsMark props = UnicodeProps.isUnicodeMark props
            member _.Mcc props = UnicodeProps.modifiedCombiningClass props
            member this.MightCompose codepoint =
                match ValueOption.isSome this.Plan.Unicode || Unicode.mightBeSecond codepoint with
                | true -> true
                | false ->
                    match this.Plan.Shaper.Kind with
                    | HebrewShaper -> codepoint >= HebrewPointFirst && codepoint <= HebrewPointLast
                    | IndicShaper -> codepoint = BengaliNukta
                    | DefaultShaper | DumberShaper | ThaiShaper | HangulShaper | KhmerShaper | UseShaper
                    | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper -> false
            member this.ReordersSingleMarks =
                match this.Plan.Shaper.Kind with
                | HebrewShaper | ArabicShaper -> true
                | DefaultShaper | DumberShaper | ThaiShaper | HangulShaper | IndicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper | UseShaper -> false
            member _.Scratch (info, props) = scratchOfGlyph info props
            member _.ScratchOf (codepoint, unicode) = scratchOfCode codepoint unicode
            member _.IsMarkOf unicode = UnicodeProps.isMarkValue unicode
            member this.Decompose (codepoint, shortest, parts) = decomposeInto this.Plan this.Font shortest codepoint parts 0
            member this.PartProps codepoint = propsOf this.Plan codepoint
            member this.Compose (starter, mark) =
                match ShaperHooks.compose this.Plan starter mark with
                | ValueNone -> ValueNone
                | ValueSome composed ->
                    match Font.nominalGlyph this.Font composed with
                    | ValueNone -> ValueNone
                    | ValueSome glyph -> ValueSome (struct (composed, glyph))
            member this.ComposedProps composed =
                match this.Plan.Unicode with
                | ValueNone ->
                    let struct (props, _) = UnicodeProps.computeLow composed
                    props
                | ValueSome _ -> propsOf this.Plan composed
            member this.Reorder (props, infos, start, finish) = ShaperHooks.reorderEdits this.Plan props infos start finish
            member _.Unhide (codepoint, previous, current, next) =
                match codepoint = Cgj with
                | false -> current
                | true ->
                    let nextCc = UnicodeProps.modifiedCombiningClass next
                    let prevCc = UnicodeProps.modifiedCombiningClass previous
                    match nextCc = 0uy || prevCc <= nextCc with
                    | true -> UnicodeProps.unhide current
                    | false -> current

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> on a run that has a mark after its first glyph and no variation
    /// selector, as one machine with no edit per glyph and no copy of the run per mark run: the result of <see cref="runPass"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, successful, not empty.</param>
    /// <returns>The normalized run.</returns>
    let internal runLean plan font run =
        let normalization = mode plan
        let always = normalization = NormalizationNone
        let might =
            always
            || (normalization <> NormalizationDecomposed && normalization <> NormalizationComposedDiacriticsNoShortCircuit)
        let composes = normalization = NormalizationComposedDiacritics || normalization = NormalizationComposedDiacriticsNoShortCircuit
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
        let result = Sealed.normalizeRun (LeanDecisions(plan, font, run.NotFound, run.Invisible)) input might always composes MaxCombiningMarks
        let scratch = scanScratchQuick (result.Infos.AsSpan()) (result.Props.AsSpan()) 0 ScratchFlags.Default
        { run with
            Infos = result.Infos
            Props = result.Props
            Positions = result.Positions
            HavePositions = result.HavePositions
            MaxOps = result.MaxOps
            Successful = result.Successful
            ScratchFlags = run.ScratchFlags ||| scratch }

    /// <summary>Whether a plan's normalization mode lets a glyph the font maps short-circuit the decomposition of a simple run,
    /// <c>might_short_circuit</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>True unless the mode decomposes fully or composes without short-circuiting.</returns>
    let internal mightShortCircuit plan =
        match mode plan with
        | NormalizationNone -> true
        | NormalizationDecomposed | NormalizationComposedDiacriticsNoShortCircuit -> false
        | NormalizationAuto | NormalizationComposedDiacritics -> true

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c>; a run whose every cluster is simple maps its
    /// nominal glyphs in one batch lookup instead of driving the out-buffer machine.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The normalized run.</returns>
    let run plan font run =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let normalization = mode plan
            let alwaysShortCircuit = normalization = NormalizationNone
            let mightShortCircuit =
                alwaysShortCircuit
                || (normalization <> NormalizationDecomposed && normalization <> NormalizationComposedDiacriticsNoShortCircuit)
            let allSimple = not (hasMarkFrom (run.Props.AsSpan()) 1)
            match not allSimple && run.Successful && ShaperHooks.leanNormalisation plan && not (hasVariationSelector (run.Infos.AsSpan()) 0) with
            | true -> runLean plan font run
            | false ->
                let decomposed =
                    match allSimple && mightShortCircuit with
                    | true -> decomposeSimple plan font alwaysShortCircuit mightShortCircuit run
                    | false -> decomposeRound plan font alwaysShortCircuit mightShortCircuit run
                finishRounds plan font allSimple normalization decomposed

    /// <summary>Copies the normalized glyph indices into the code points, <c>hb_ot_map_glyphs_fast</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with glyph content.</returns>
    let mapGlyphsFast (run: GlyphRun) =
        let infos = Sealed.mapi (fun index (info: GlyphInfo) -> { info with Codepoint = run.Props[index].GlyphIndex }) (run.Infos.AsSpan())
        { run with Infos = infos; ContentType = GlyphContent }

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> as <see cref="runLean"/> does, on a run whose arrays the caller made
    /// in this call and has shared with no one: the glyphs the rounds leave where they are stay in those arrays.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, successful, not empty, with owned arrays.</param>
    /// <returns>The normalized run.</returns>
    let internal runLeanOwned plan font run =
        let normalization = mode plan
        let always = normalization = NormalizationNone
        let might =
            always
            || (normalization <> NormalizationDecomposed && normalization <> NormalizationComposedDiacriticsNoShortCircuit)
        let composes = normalization = NormalizationComposedDiacritics || normalization = NormalizationComposedDiacriticsNoShortCircuit
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
        let decisions = LeanDecisions(plan, font, run.NotFound, run.Invisible)
        match Sealed.normalizeFlatOwned decisions input might always composes MaxCombiningMarks with
        | ValueSome (struct (flat, true)) -> GlyphRun.adopt run flat
        | ValueSome (struct (flat, false)) ->
            let scratch = scanScratchQuick (flat.Infos.AsSpan()) (flat.Props.AsSpan()) 0 ScratchFlags.Default
            GlyphRun.adopt run { flat with ScratchFlags = flat.ScratchFlags ||| scratch }
        | ValueNone ->
            let result = Sealed.normalizeRunOwned decisions input might always composes MaxCombiningMarks
            let scratch = scanScratchQuick (result.Infos.AsSpan()) (result.Props.AsSpan()) 0 ScratchFlags.Default
            GlyphRun.adopt run { result with ScratchFlags = run.ScratchFlags ||| scratch }

    /// <summary>Runs the normaliser on a run with arrays of its own that the lean normaliser does not take: a run whose every cluster is
    /// simple and whose font maps every code point has its glyphs written in place, and any other takes <see cref="run"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="owned">The run, with owned arrays.</param>
    /// <param name="allSimple">Whether the run has no mark after its first glyph.</param>
    /// <returns>The normalized run.</returns>
    let private simpleOwned plan font (owned: GlyphRun) allSimple =
        let input =
            { Infos = owned.Infos
              Props = owned.Props
              Positions = owned.Positions
              HavePositions = owned.HavePositions
              MaxLen = owned.MaxLen
              MaxOps = owned.MaxOps
              Successful = owned.Successful
              ClusterLevel = owned.ClusterLevel
              Flags = owned.Flags
              Serial = byte owned.Serial
              RandomState = owned.RandomState
              ScratchFlags = owned.ScratchFlags }
        let simple =
            match allSimple && mightShortCircuit plan with
            | true -> Sealed.normalizeSimpleOwned (LeanDecisions(plan, font, owned.NotFound, owned.Invisible)) input
            | false -> ValueNone
        match simple with
        | ValueSome result -> GlyphRun.adopt owned result
        | ValueNone -> run plan font owned

    /// <summary>Runs the normaliser on a working set that the lean normaliser does not take, as <see cref="simpleOwned"/> does a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with owned arrays.</param>
    /// <param name="allSimple">Whether the set has no mark after its first glyph.</param>
    /// <returns>The normalized set.</returns>
    let private simpleOwnedSet plan font (set: WorkingSet) allSimple =
        let simple =
            match allSimple && mightShortCircuit plan with
            | true -> Sealed.normalizeSimpleOwned (LeanDecisions(plan, font, set.NotFound, set.Invisible)) (GlyphRun.inputOfSet set)
            | false -> ValueNone
        match simple with
        | ValueSome result -> GlyphRun.applyResult set result
        | ValueNone -> GlyphRun.setOf (run plan font (GlyphRun.runOf set))

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> as <see cref="run"/> does, on a run whose arrays the caller made in
    /// this call and has shared with no one: a run with marks goes through <see cref="runLeanOwned"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="owned">The run, with owned arrays.</param>
    /// <returns>The normalized run.</returns>
    let internal runOwned plan font owned =
        match owned.Infos.Length with
        | 0 -> owned
        | _ ->
            let allSimple = not (hasMarkFrom (owned.Props.AsSpan()) 1)
            match not allSimple && owned.Successful && ShaperHooks.leanNormalisation plan && not (hasVariationSelector (owned.Infos.AsSpan()) 0) with
            | true -> runLeanOwned plan font owned
            | false -> simpleOwned plan font owned allSimple

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> as <see cref="runLeanOwned"/> does, on a working set whose arrays the
    /// caller made in this call and has shared with no one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, successful, not empty.</param>
    /// <returns>The normalized set.</returns>
    let internal runLeanOwnedSet plan font (set: WorkingSet) =
        let normalization = mode plan
        let always = normalization = NormalizationNone
        let might =
            always
            || (normalization <> NormalizationDecomposed && normalization <> NormalizationComposedDiacriticsNoShortCircuit)
        let composes = normalization = NormalizationComposedDiacritics || normalization = NormalizationComposedDiacriticsNoShortCircuit
        let decisions = LeanDecisions(plan, font, set.NotFound, set.Invisible)
        let input = GlyphRun.inputOfSet set
        match Sealed.normalizeFlatOwned decisions input might always composes MaxCombiningMarks with
        | ValueSome (struct (flat, true)) -> GlyphRun.applyResult set flat
        | ValueSome (struct (flat, false)) ->
            let scratch = scanScratchQuick (flat.Infos.AsSpan()) (flat.Props.AsSpan()) 0 ScratchFlags.Default
            GlyphRun.applyResult set { flat with ScratchFlags = flat.ScratchFlags ||| scratch }
        | ValueNone ->
            let result = Sealed.normalizeRunOwned decisions input might always composes MaxCombiningMarks
            let scratch = scanScratchQuick (result.Infos.AsSpan()) (result.Props.AsSpan()) 0 ScratchFlags.Default
            GlyphRun.applyResult set { result with ScratchFlags = set.ScratchFlags ||| scratch }

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> as <see cref="runOwned"/> does, on a working set whose arrays the caller
    /// made in this call and has shared with no one: a set with marks goes through <see cref="runLeanOwnedSet"/>, and a set the lean
    /// normaliser does not read takes the rounds on a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with owned arrays.</param>
    /// <returns>The normalized set.</returns>
    let internal runOwnedSet plan font (set: WorkingSet) =
        match set.Infos.Length with
        | 0 -> set
        | _ ->
            let allSimple = not (hasMarkFrom (set.Props.AsSpan()) 1)
            match not allSimple && set.Successful && ShaperHooks.leanNormalisation plan && not (hasVariationSelector (set.Infos.AsSpan()) 0) with
            | true -> runLeanOwnedSet plan font set
            | false -> simpleOwnedSet plan font set allSimple
