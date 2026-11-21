namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Unicode-property and synthesized glyph-class stages of the ot pipeline.</summary>
[<RequireQualifiedAccess>]
module internal OtProps =
    /// <summary>The first emoji modifier, <c>0x1F3FB</c>.</summary>
    [<Literal>]
    let private EmojiModifierFirst = 0x1F3FBu

    /// <summary>The last emoji modifier, <c>0x1F3FF</c>.</summary>
    [<Literal>]
    let private EmojiModifierLast = 0x1F3FFu

    /// <summary>The first regional indicator, <c>0x1F1E6</c>.</summary>
    [<Literal>]
    let private RegionalIndicatorFirst = 0x1F1E6u

    /// <summary>The last regional indicator, <c>0x1F1FF</c>.</summary>
    [<Literal>]
    let private RegionalIndicatorLast = 0x1F1FFu

    /// <summary>The first halfwidth katakana extend character, <c>0xFF9E</c>.</summary>
    [<Literal>]
    let private HalfwidthFirst = 0xFF9Eu

    /// <summary>The last halfwidth katakana extend character, <c>0xFF9F</c>.</summary>
    [<Literal>]
    let private HalfwidthLast = 0xFF9Fu

    /// <summary>The first tag character, <c>0xE0020</c>.</summary>
    [<Literal>]
    let private TagFirst = 0xE0020u

    /// <summary>The last tag character, <c>0xE007F</c>.</summary>
    [<Literal>]
    let private TagLast = 0xE007Fu

    /// <summary>The combining grapheme joiner, <c>0x034F</c>.</summary>
    [<Literal>]
    let private CgjCodepoint = 0x034Fu

    /// <summary>The fraction slash, <c>0x2044</c>.</summary>
    [<Literal>]
    let private FractionSlashCodepoint = 0x2044u

    /// <summary>The zero-width joiner, <c>0x200D</c>, the only glyph whose continuation looks at the next
    /// code point's Extended_Pictographic property.</summary>
    [<Literal>]
    let private ZwjCodepoint = 0x200Du

    /// <summary>Whether a code point is a regional indicator, <c>_hb_codepoint_is_regional_indicator</c>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True for U+1F1E6..U+1F1FF.</returns>
    let private isRegionalIndicator (codepoint: uint32) : bool =
        codepoint >= RegionalIndicatorFirst && codepoint <= RegionalIndicatorLast

    /// <summary>Whether a modifier-symbol code point is an emoji modifier.</summary>
    /// <param name="category">The general category.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True for Sk in U+1F3FB..U+1F3FF.</returns>
    let private isEmojiModifier (category: GeneralCategory) (codepoint: uint32) : bool =
        category = GeneralCategory.ModifierSymbol && codepoint >= EmojiModifierFirst && codepoint <= EmojiModifierLast

    /// <summary>Whether a code point is an Other_Grapheme_Extend tag or halfwidth katakana mark.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True for U+FF9E..U+FF9F and U+E0020..U+E007F.</returns>
    let private isTagOrHalfwidth (codepoint: uint32) : bool =
        (codepoint >= HalfwidthFirst && codepoint <= HalfwidthLast)
        || (codepoint >= TagFirst && codepoint <= TagLast)

    /// <summary>Whether a category is one upstream skips in <c>hb_set_unicode_props</c> (Lu/Ll/Lt/Lo/Zs).</summary>
    /// <param name="category">The general category.</param>
    /// <returns>True for the letters and the space separator.</returns>
    let private isLetterOrSpace (category: GeneralCategory) : bool =
        match category with
        | GeneralCategory.LowercaseLetter | GeneralCategory.UppercaseLetter | GeneralCategory.TitlecaseLetter
        | GeneralCategory.OtherLetter | GeneralCategory.SpaceSeparator -> true
        | GeneralCategory.Control | GeneralCategory.Format | GeneralCategory.Unassigned | GeneralCategory.PrivateUse
        | GeneralCategory.Surrogate | GeneralCategory.ModifierLetter | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark
        | GeneralCategory.NonSpacingMark | GeneralCategory.DecimalNumber | GeneralCategory.LetterNumber | GeneralCategory.OtherNumber
        | GeneralCategory.ConnectPunctuation | GeneralCategory.DashPunctuation | GeneralCategory.ClosePunctuation
        | GeneralCategory.FinalPunctuation | GeneralCategory.InitialPunctuation | GeneralCategory.OtherPunctuation
        | GeneralCategory.OpenPunctuation | GeneralCategory.CurrencySymbol | GeneralCategory.ModifierSymbol
        | GeneralCategory.MathSymbol | GeneralCategory.OtherSymbol | GeneralCategory.LineSeparator | GeneralCategory.ParagraphSeparator -> false

    /// <summary>The fold state of <c>hb_set_unicode_props</c>: the current index, whether the previous glyph is a continuation and whether to skip the next glyph's rules.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private PropsState =
        { Index: int
          PreviousContinuation: bool
          SkipNext: bool }

    /// <summary>The continuation decision of one glyph: whether to mark it a continuation and whether to skip the next glyph's rules.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="previousCodepoint">The previous code point, 0 at the start.</param>
    /// <param name="nextIsExtPict">Whether the next code point is Extended_Pictographic.</param>
    /// <param name="previousContinuation">Whether the previous glyph is a continuation.</param>
    /// <param name="category">The general category.</param>
    /// <param name="props">The computed base properties.</param>
    /// <returns>The continuation and skip-next decisions.</returns>
    let private ruleDecision
            (codepoint: uint32)
            (previousCodepoint: uint32)
            (nextIsExtPict: bool)
            (previousContinuation: bool)
            (category: GeneralCategory)
            (props: GlyphProps)
            : struct (bool * bool) =
        let emojiModifier = isEmojiModifier category codepoint
        let regional = isRegionalIndicator codepoint
        let regionalContinued = regional && isRegionalIndicator previousCodepoint && not previousContinuation
        let zwj = UnicodeProps.isZwj props
        let tag = isTagOrHalfwidth codepoint
        match struct (emojiModifier, regional, regionalContinued, zwj, tag) with
        | struct (true, _, _, _, _) -> struct (true, false)
        | struct (false, true, true, _, _) -> struct (true, false)
        | struct (false, true, false, _, _) -> struct (false, false)
        | struct (false, false, _, true, _) -> struct (true, nextIsExtPict)
        | struct (false, false, _, false, true) -> struct (true, false)
        | struct (false, false, _, false, false) -> struct (false, false)

    /// <summary>Computes the properties of one glyph, applying the continuation rules of <c>hb_set_unicode_props</c>.</summary>
    /// <param name="unicode">The packed Unicode properties of the code point.</param>
    /// <param name="codepoint">The code point.</param>
    /// <param name="previousCodepoint">The previous code point, 0 at the start.</param>
    /// <param name="nextIsExtPict">Whether the next code point is Extended_Pictographic.</param>
    /// <param name="state">The fold state.</param>
    /// <returns>The glyph properties and the next state.</returns>
    let private propsStepFrom
            (unicode: uint16)
            (codepoint: uint32)
            (previousCodepoint: uint32)
            (nextIsExtPict: bool)
            (state: PropsState)
            : struct (GlyphProps * PropsState) =
        let baseProps = { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
        let next = { state with Index = state.Index + 1 }
        match state.SkipNext with
        | true ->
            let props = { baseProps with UnicodeProps = unicode ||| UnicodeProps.Continuation }
            struct (props, { next with SkipNext = false; PreviousContinuation = true })
        | false ->
            match codepoint >= 0x80u with
            | false -> struct (baseProps, { next with PreviousContinuation = UnicodeProps.isContinuation baseProps })
            | true ->
                let category = UnicodeProps.generalCategory baseProps
                match isLetterOrSpace category with
                | true -> struct (baseProps, { next with PreviousContinuation = UnicodeProps.isContinuation baseProps })
                | false ->
                    let struct (continuation, skipNext) =
                        ruleDecision codepoint previousCodepoint nextIsExtPict state.PreviousContinuation category baseProps
                    let finalUnicode = if continuation then unicode ||| UnicodeProps.Continuation else unicode
                    let props = { baseProps with UnicodeProps = finalUnicode }
                    struct (props, { next with SkipNext = skipNext; PreviousContinuation = UnicodeProps.isContinuation props })

    /// <summary>Computes the properties of one glyph from the default Unicode functions, <see cref="propsStepFrom"/>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="previousCodepoint">The previous code point, 0 at the start.</param>
    /// <param name="nextIsExtPict">Whether the next code point is Extended_Pictographic.</param>
    /// <param name="state">The fold state.</param>
    /// <returns>The glyph properties and the next state.</returns>
    let private propsStep (codepoint: uint32) (previousCodepoint: uint32) (nextIsExtPict: bool) (state: PropsState) =
        let struct (unicode, _) = UnicodeProps.computeLow codepoint
        propsStepFrom unicode codepoint previousCodepoint nextIsExtPict state

    /// <summary>Unfolds the glyph properties of a whole run through <c>hb_set_unicode_props</c> with replaced Unicode functions.</summary>
    [<Struct>]
    type private PropsFolderWith =
        val Funcs: UnicodeFuncs
        new (funcs) = { Funcs = funcs }

        interface ISpanUnfolder<GlyphInfo, PropsState, GlyphProps> with
            member this.Next (source, state) =
                let index = state.Index
                let codepoint = source[index].Codepoint
                let previousCodepoint = if index > 0 then source[index - 1].Codepoint else 0u
                let nextIsExtPict =
                    match codepoint = ZwjCodepoint with
                    | false -> false
                    | true -> index + 1 < source.Length && Unicode.isExtendedPictographic source[index + 1].Codepoint
                let struct (unicode, _) = UnicodeProps.computeWith this.Funcs codepoint
                propsStepFrom unicode codepoint previousCodepoint nextIsExtPict state

    /// <summary>Unfolds the glyph properties of a whole run through <c>hb_set_unicode_props</c>.</summary>
    [<Struct>]
    type private PropsFolder =
        interface ISpanUnfolder<GlyphInfo, PropsState, GlyphProps> with
            member _.Next (source, state) =
                let index = state.Index
                let codepoint = source[index].Codepoint
                let previousCodepoint = if index > 0 then source[index - 1].Codepoint else 0u
                let nextIsExtPict =
                    match codepoint = ZwjCodepoint with
                    | false -> false
                    | true -> index + 1 < source.Length && Unicode.isExtendedPictographic source[index + 1].Codepoint
                let struct (props, next) = propsStep codepoint previousCodepoint nextIsExtPict state
                struct (props, next)

    /// <summary>Ors the scratch flags of a shaped run back in, <c>hb_set_unicode_props</c>'s side effects.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The computed properties.</param>
    /// <param name="index">The next index.</param>
    /// <param name="scratch">The flags so far.</param>
    /// <returns>The accumulated flags.</returns>
    [<TailCall>]
    let rec private accumulateScratch (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) (scratch: ScratchFlags) : ScratchFlags =
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
                | CgjCodepoint -> scratch ||| ScratchFlags.HasCgj
                | FractionSlashCodepoint -> scratch ||| ScratchFlags.HasFractionSlash
                | _ -> scratch
            accumulateScratch infos props (index + 1) scratch

    /// <summary>Sets every glyph's mask and Unicode properties, <c>hb_set_unicode_props</c>.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the mask and properties set.</returns>
    let setUnicodeProps (globalMask: uint32) (run: GlyphRun) : GlyphRun =
        let infos = Sealed.map (fun (info: GlyphInfo) -> { info with Mask = globalMask }) (run.Infos.AsSpan())
        let props = Sealed.unfoldSpan (run.Infos.AsSpan()) run.Infos.Length { Index = 0; PreviousContinuation = false; SkipNext = false } (PropsFolder())
        let scratch = accumulateScratch (run.Infos.AsSpan()) (props.AsSpan()) 0 run.ScratchFlags
        { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Sets every glyph's mask and Unicode properties through replaced Unicode functions, <c>hb_set_unicode_props</c> with the
    /// functions of the buffer.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the mask and properties set.</returns>
    let setUnicodePropsWith (funcs: UnicodeFuncs) (globalMask: uint32) (run: GlyphRun) =
        let infos = Sealed.map (fun (info: GlyphInfo) -> { info with Mask = globalMask }) (run.Infos.AsSpan())
        let props = Sealed.unfoldSpan (run.Infos.AsSpan()) run.Infos.Length { Index = 0; PreviousContinuation = false; SkipNext = false } (PropsFolderWith funcs)
        let scratch = accumulateScratch (run.Infos.AsSpan()) (props.AsSpan()) 0 run.ScratchFlags
        { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Synthesizes the glyph class of one glyph, <c>hb_synthesize_glyph_classes</c>' per-glyph switch.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The properties with the synthesized class.</returns>
    let private synthesizeClass (props: GlyphProps) : GlyphProps =
        match not (UnicodeProps.isNonSpacingMark props) || UnicodeProps.isDefaultIgnorable props with
        | true -> { props with GlyphClass = GlyphPropsBits.Base }
        | false -> { props with GlyphClass = GlyphPropsBits.Mark }

    /// <summary>Synthesizes base/mark glyph classes without GDEF, <c>hb_synthesize_glyph_classes</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the synthesized classes.</returns>
    let synthesizeGlyphClasses (run: GlyphRun) : GlyphRun =
        { run with Props = Sealed.mapIfChanged synthesizeClass run.Props }

    /// <summary>The dotted circle, <c>0x25CC</c>.</summary>
    [<Literal>]
    let private DottedCircle = 0x25CCu

    /// <summary>The state of the dotted-circle pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DottedCircleStep =
        | CircleClear
        | CircleInsert
        | CircleDone

    /// <summary>The out-buffer pass that inserts a dotted circle, <c>hb_insert_dotted_circle</c>.</summary>
    [<Struct>]
    type private DottedCirclePass =
        val Circle: GlyphInfo
        val CircleProps: GlyphProps

        /// <summary>Creates a dotted-circle pass.</summary>
        /// <param name="circle">The circle glyph.</param>
        /// <param name="circleProps">Its properties.</param>
        new (circle, circleProps) = { Circle = circle; CircleProps = circleProps }

        interface IRunPass<DottedCircleStep> with
            member this.Step (cursor, state) =
                match state with
                | CircleClear -> struct (RunEdit.ClearOutput, CircleInsert)
                | CircleInsert ->
                    let info = { this.Circle with Cluster = cursor.Input[0].Cluster; Mask = cursor.Input[0].Mask }
                    struct (RunEdit.OutputInfo (info, this.CircleProps), CircleDone)
                | CircleDone -> struct (RunEdit.Finish, CircleDone)

    /// <summary>Whether the first glyph of the run is a Unicode mark, <c>hb-ot-shape.cc:559</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>True when the first glyph is a mark.</returns>
    let private startsWithMark (run: GlyphRun) : bool =
        run.Props.Length > 0 && UnicodeProps.isUnicodeMark run.Props[0]

    /// <summary>Inserts a dotted circle before a leading mark, <c>hb_insert_dotted_circle</c>, the circle carrying the given properties.</summary>
    /// <param name="circleUnicode">Answers the packed Unicode properties of the dotted circle, asked only when a circle is inserted.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run, with a dotted circle prepended when the conditions hold.</returns>
    let private insertDottedCircleOf (circleUnicode: unit -> uint16) (font: Font) (run: GlyphRun) =
        match struct (run.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default,
                      run.Flags &&& BufferFlags.Bot <> BufferFlags.Default,
                      run.ContextBefore.IsEmpty,
                      startsWithMark run,
                      Font.nominalGlyph font DottedCircle) with
        | struct (true, true, true, true, ValueSome _) ->
            let circle = { Codepoint = DottedCircle; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u }
            let circleProps = { Unchecked.defaultof<GlyphProps> with UnicodeProps = circleUnicode () }
            GlyphRun.runPass (DottedCirclePass (circle, circleProps)) CircleClear run
        | struct (_, _, _, _, _) -> run

    /// <summary>Inserts a dotted circle before a leading mark, <c>hb_insert_dotted_circle</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run, with a dotted circle prepended when the conditions hold.</returns>
    let insertDottedCircle (font: Font) (run: GlyphRun) =
        insertDottedCircleOf (fun () -> let struct (props, _) = UnicodeProps.compute DottedCircle in props) font run

    /// <summary>Inserts a dotted circle before a leading mark, <c>hb_insert_dotted_circle</c>, through replaced Unicode functions.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run, with a dotted circle prepended when the conditions hold.</returns>
    let insertDottedCircleWith (funcs: UnicodeFuncs) (font: Font) (run: GlyphRun) =
        insertDottedCircleOf (fun () -> let struct (props, _) = UnicodeProps.computeWith funcs DottedCircle in props) font run

    /// <summary>The grapheme grouping: a glyph and its following continuations, <c>_hb_grapheme_group_func</c>.</summary>
    [<Struct>]
    type private GraphemeGrouping =
        interface IGlyphGrouping with
            member _.Group (_, current) = UnicodeProps.isContinuation current

    /// <summary>The state of the cluster-forming pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FormClustersState =
        | FormNext of start: int
        | FormDone

    /// <summary>Finds the end of a grapheme: the first non-continuation after the start, <c>group_end</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The index after the group's first glyph.</param>
    /// <returns>The exclusive end index of the grapheme.</returns>
    [<TailCall>]
    let rec private graphemeEnd (props: ReadOnlySpan<GlyphProps>) (index: int) : int =
        match index < props.Length && UnicodeProps.isContinuation props[index] with
        | true -> graphemeEnd props (index + 1)
        | false -> index

    /// <summary>The out-buffer pass that merges each grapheme's clusters, <c>hb_form_clusters</c>.</summary>
    [<Struct>]
    type private FormClustersPass =
        interface IRunPass<FormClustersState> with
            member _.Step (cursor, state) =
                match state with
                | FormDone -> struct (RunEdit.Finish, FormDone)
                | FormNext start ->
                    match start >= cursor.Input.Length with
                    | true -> struct (RunEdit.Finish, FormDone)
                    | false ->
                        let finish = graphemeEnd cursor.InputProps (start + 1)
                        struct (RunEdit.MergeGraphemeClusters (start, finish), FormNext finish)

    /// <summary>Merges the clusters of every grapheme through the out-buffer pass, <c>hb_form_clusters</c>: the reference
    /// <see cref="formClusters"/> must equal.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with grapheme clusters merged.</returns>
    let internal formClustersPass (run: GlyphRun) =
        match run.ScratchFlags &&& ScratchFlags.HasContinuations = ScratchFlags.Default with
        | true -> run
        | false -> GlyphRun.runPass (FormClustersPass()) (FormNext 0) run

    /// <summary>Merges the clusters of every grapheme, <c>hb_form_clusters</c>, in one loop over the graphemes.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with grapheme clusters merged.</returns>
    let formClusters (run: GlyphRun) =
        match run.ScratchFlags &&& ScratchFlags.HasContinuations = ScratchFlags.Default with
        | true -> run
        | false -> GlyphRun.mergeGroups (GraphemeGrouping()) run

    /// <summary>Whether a direction is vertical, <c>HB_DIRECTION_IS_VERTICAL</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for TTB and BTT.</returns>
    let private isVertical (direction: Direction) : bool =
        match direction with
        | Direction.Ttb | Direction.Btt -> true
        | Direction.Invalid | Direction.Ltr | Direction.Rtl -> false

    /// <summary>Whether a general category is a letter, <c>HB_UNICODE_GENERAL_CATEGORY_IS_LETTER</c>.</summary>
    /// <param name="category">The category.</param>
    /// <returns>True for Lu, Ll, Lt, Lm and Lo.</returns>
    let private isLetter (category: GeneralCategory) : bool =
        match category with
        | GeneralCategory.LowercaseLetter | GeneralCategory.ModifierLetter | GeneralCategory.OtherLetter
        | GeneralCategory.TitlecaseLetter | GeneralCategory.UppercaseLetter -> true
        | GeneralCategory.Control | GeneralCategory.Format | GeneralCategory.Unassigned | GeneralCategory.PrivateUse
        | GeneralCategory.Surrogate | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark
        | GeneralCategory.DecimalNumber | GeneralCategory.LetterNumber | GeneralCategory.OtherNumber
        | GeneralCategory.ConnectPunctuation | GeneralCategory.DashPunctuation | GeneralCategory.ClosePunctuation
        | GeneralCategory.FinalPunctuation | GeneralCategory.InitialPunctuation | GeneralCategory.OtherPunctuation
        | GeneralCategory.OpenPunctuation | GeneralCategory.CurrencySymbol | GeneralCategory.ModifierSymbol
        | GeneralCategory.MathSymbol | GeneralCategory.OtherSymbol | GeneralCategory.LineSeparator
        | GeneralCategory.ParagraphSeparator | GeneralCategory.SpaceSeparator -> false

    /// <summary>The digits-and-regional-indicator scan of <c>hb_ensure_native_direction</c>.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DirectionScan =
        { Letter: bool
          Number: bool
          Regional: bool }

    /// <summary>Scans for a letter, a decimal number or a regional indicator, <c>hb-ot-shape.cc:618-630</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="index">The next index.</param>
    /// <param name="state">The findings so far.</param>
    /// <returns>The findings.</returns>
    [<TailCall>]
    let rec private scanDirection (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) (index: int) (state: DirectionScan) : DirectionScan =
        match index >= infos.Length with
        | true -> state
        | false ->
            let category = UnicodeProps.generalCategory props[index]
            match isLetter category with
            | true -> { state with Letter = true }
            | false ->
                let state =
                    match category = GeneralCategory.DecimalNumber with
                    | true -> { state with Number = true }
                    | false -> state
                let state =
                    match isRegionalIndicator infos[index].Codepoint with
                    | true -> { state with Regional = true }
                    | false -> state
                scanDirection props infos (index + 1) state

    /// <summary>Reverses graphemes and flips the direction when it differs from the native one, <c>hb_ensure_native_direction</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run in its native direction.</returns>
    let ensureNativeDirection (run: GlyphRun) : GlyphRun =
        let direction = run.SegmentProperties.Direction
        let horizDir = Script.horizontalDirection run.SegmentProperties.Script
        let horizDir =
            match horizDir = Direction.Rtl && direction = Direction.Ltr with
            | true ->
                let scan = scanDirection (run.Props.AsSpan()) (run.Infos.AsSpan()) 0 { Letter = false; Number = false; Regional = false }
                match (scan.Number || scan.Regional) && not scan.Letter with
                | true -> Direction.Ltr
                | false -> horizDir
            | false -> horizDir
        let shouldReverse =
            (Direction.isHorizontal direction && direction <> horizDir && horizDir <> Direction.Invalid)
            || (isVertical direction && direction <> Direction.Ttb)
        match shouldReverse with
        | true ->
            let mergeClusters = run.ClusterLevel = ClusterLevel.MonotoneCharacters
            GlyphRun.withDirection (Direction.reverse direction) (GlyphRun.reverseGroups (GraphemeGrouping()) mergeClusters run)
        | false -> run

    /// <summary>The state of the fused properties pass: the unfold state plus the accumulated scratch flags.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FusedPropsState =
        { Props: PropsState
          Scratch: ScratchFlags }

    /// <summary>The folder that sets every glyph's mask and Unicode properties in one pass, accumulating the scratch flags.</summary>
    [<Struct>]
    type private PropsUnfolder =
        val GlobalMask: uint32

        /// <summary>Creates the folder.</summary>
        /// <param name="globalMask">The mask assigned to every glyph.</param>
        new (globalMask) = { GlobalMask = globalMask }

        interface Sealed.IPairUnfolder<GlyphInfo, FusedPropsState, GlyphInfo, GlyphProps> with
            member this.Next (source, state) =
                let propsState = state.Props
                let index = propsState.Index
                let info = source[index]
                let codepoint = info.Codepoint
                let previousCodepoint = if index > 0 then source[index - 1].Codepoint else 0u
                let nextIsExtPict =
                    match codepoint = ZwjCodepoint with
                    | false -> false
                    | true -> index + 1 < source.Length && Unicode.isExtendedPictographic source[index + 1].Codepoint
                let struct (props, nextProps) = propsStep codepoint previousCodepoint nextIsExtPict propsState
                let scratch =
                    match UnicodeProps.isContinuation props with
                    | true -> state.Scratch ||| ScratchFlags.HasContinuations
                    | false -> state.Scratch
                let scratch =
                    match props.UnicodeProps &&& UnicodeProps.Ignorable <> 0us with
                    | true -> scratch ||| ScratchFlags.HasDefaultIgnorables
                    | false -> scratch
                let scratch =
                    match codepoint with
                    | CgjCodepoint -> scratch ||| ScratchFlags.HasCgj
                    | FractionSlashCodepoint -> scratch ||| ScratchFlags.HasFractionSlash
                    | _ -> scratch
                struct ({ info with Mask = this.GlobalMask }, props, { Props = nextProps; Scratch = scratch })

    /// <summary>Sets every glyph's mask and Unicode properties in one pass through the unfolder, any code point.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the mask and properties set.</returns>
    let private setUnicodePropsUnfolded globalMask (run: GlyphRun) =
        let struct (infos, props, final) =
            Sealed.unfoldPair (run.Infos.AsSpan()) run.Infos.Length { Props = { Index = 0; PreviousContinuation = false; SkipNext = false }; Scratch = run.ScratchFlags } (PropsUnfolder(globalMask))
        { run with Infos = infos; Props = props; ScratchFlags = final.Scratch }


    /// <summary>The first precomposed Hangul syllable, <c>SBase</c>.</summary>
    [<Literal>]
    let private HangulSyllableFirst = 0xAC00u

    /// <summary>The last precomposed Hangul syllable.</summary>
    [<Literal>]
    let private HangulSyllableLast = 0xD7A3u

    /// <summary>What <see cref="LetterProps"/> answers for every precomposed Hangul syllable: they are all letters of one category with no
    /// combining class and no ignorable bit, so the answer of the first is the answer of each.</summary>
    let private hangulSyllablePlain =
        let struct (unicode, scratch) = UnicodeProps.compute HangulSyllableFirst
        let category = UnicodeProps.generalCategory { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
        struct (isLetterOrSpace category, unicode, scratch)

    /// <summary>The properties of a code point beyond the small table, for the lines of letters: plain when the code point is a letter or a
    /// space, whose properties <see cref="propsStep"/> takes from the table unchanged.</summary>
    [<Struct>]
    type private LetterProps =
        interface IPlainProps with
            member _.Plain codepoint =
                match codepoint - HangulSyllableFirst <= HangulSyllableLast - HangulSyllableFirst with
                | true -> hangulSyllablePlain
                | false ->
                    let struct (unicode, scratch) = UnicodeProps.compute codepoint
                    let category = UnicodeProps.generalCategory { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
                    struct (isLetterOrSpace category, unicode, scratch)

    /// <summary>Prepares a run for shaping and sets every glyph's mask and Unicode properties, <c>hb_buffer_enter</c> then
    /// <see cref="setUnicodePropsFused"/>, in one run record instead of two.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run as it came in.</param>
    /// <returns>The entered run with the mask and properties set, whose arrays this call made.</returns>
    let setUnicodePropsEntered globalMask (run: GlyphRun) =
        let entered infos props scratch =
            { run with Infos = infos; Props = props; ScratchFlags = scratch; MaxLen = GlyphRun.enteredMaxLen run; MaxOps = GlyphRun.enteredMaxOps run; Serial = 0u }
        match Sealed.asciiProps UnicodeProps.latinTable globalMask UnicodeProps.Continuation UnicodeProps.Ignorable (run.Infos.AsSpan()) with
        | ValueNone ->
            match Sealed.lowProps UnicodeProps.lowTable globalMask ScratchFlags.Default ZwjCodepoint FractionSlashCodepoint (run.Infos.AsSpan()) with
            | ValueNone ->
                match Sealed.lettersProps UnicodeProps.lowTable globalMask ZwjCodepoint FractionSlashCodepoint (LetterProps()) (run.Infos.AsSpan()) with
                | ValueNone -> setUnicodePropsUnfolded globalMask (GlyphRun.enter run)
                | ValueSome (struct (infos, props, scratch)) -> entered infos props scratch
            | ValueSome (struct (infos, props, scratch)) -> entered infos props scratch
        | ValueSome (struct (infos, props, continuations, ignorables)) ->
            let withContinuations =
                match continuations with
                | true -> ScratchFlags.Default ||| ScratchFlags.HasContinuations
                | false -> ScratchFlags.Default
            let scratch =
                match ignorables with
                | true -> withContinuations ||| ScratchFlags.HasDefaultIgnorables
                | false -> withContinuations
            entered infos props scratch

    /// <summary>Sets every glyph's mask and Unicode properties in one pass through the unfolder, any code point, for a run that is entered,
    /// <see cref="setUnicodePropsUnfolded"/> into a working set.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run as it came in.</param>
    /// <returns>The working set with the mask and properties set.</returns>
    let private setUnicodePropsUnfoldedSet globalMask (run: GlyphRun) =
        let struct (infos, props, final) =
            Sealed.unfoldPair (run.Infos.AsSpan()) run.Infos.Length { Props = { Index = 0; PreviousContinuation = false; SkipNext = false }; Scratch = ScratchFlags.Default } (PropsUnfolder(globalMask))
        GlyphRun.enteredSet run infos props final.Scratch

    /// <summary>Prepares a run for shaping and sets every glyph's mask and Unicode properties as <see cref="setUnicodePropsEntered"/> does,
    /// into a working set: the arrays this call makes are the only allocation of the stage, and no run record is built.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run as it came in.</param>
    /// <returns>The working set, whose arrays this call made.</returns>
    let setUnicodePropsEnteredSet globalMask (run: GlyphRun) =
        match Sealed.asciiProps UnicodeProps.latinTable globalMask UnicodeProps.Continuation UnicodeProps.Ignorable (run.Infos.AsSpan()) with
        | ValueNone ->
            match Sealed.lowProps UnicodeProps.lowTable globalMask ScratchFlags.Default ZwjCodepoint FractionSlashCodepoint (run.Infos.AsSpan()) with
            | ValueNone ->
                match Sealed.lettersProps UnicodeProps.lowTable globalMask ZwjCodepoint FractionSlashCodepoint (LetterProps()) (run.Infos.AsSpan()) with
                | ValueNone -> setUnicodePropsUnfoldedSet globalMask run
                | ValueSome (struct (infos, props, scratch)) -> GlyphRun.enteredSet run infos props scratch
            | ValueSome (struct (infos, props, scratch)) -> GlyphRun.enteredSet run infos props scratch
        | ValueSome (struct (infos, props, continuations, ignorables)) ->
            let withContinuations =
                match continuations with
                | true -> ScratchFlags.Default ||| ScratchFlags.HasContinuations
                | false -> ScratchFlags.Default
            let scratch =
                match ignorables with
                | true -> withContinuations ||| ScratchFlags.HasDefaultIgnorables
                | false -> withContinuations
            GlyphRun.enteredSet run infos props scratch

    /// <summary>Prepares a run the caller decoded in this call for shaping and sets every glyph's mask and Unicode properties as
    /// <see cref="setUnicodePropsEnteredSet"/> does, writing the masks into the glyphs of the run: the properties are the one array the
    /// stage allocates. The glyphs of the run are the set's afterwards, so the run must be shared with no one and not shaped again.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run as the caller decoded it, with glyphs of its own.</param>
    /// <returns>The working set, whose glyphs are those of the run.</returns>
    let setUnicodePropsFreshSet globalMask (run: GlyphRun) =
        match Sealed.asciiPropsInPlace UnicodeProps.latinTable globalMask UnicodeProps.Continuation UnicodeProps.Ignorable run.Infos with
        | ValueNone ->
            match Sealed.lowPropsInPlace UnicodeProps.lowTable globalMask ScratchFlags.Default ZwjCodepoint FractionSlashCodepoint run.Infos with
            | ValueNone ->
                match Sealed.lettersPropsInPlace UnicodeProps.lowTable globalMask ZwjCodepoint FractionSlashCodepoint (LetterProps()) run.Infos with
                | ValueNone -> setUnicodePropsUnfoldedSet globalMask run
                | ValueSome (struct (props, scratch)) -> GlyphRun.enteredSet run run.Infos props scratch
            | ValueSome (struct (props, scratch)) -> GlyphRun.enteredSet run run.Infos props scratch
        | ValueSome (struct (props, continuations, ignorables)) ->
            let withContinuations =
                match continuations with
                | true -> ScratchFlags.Default ||| ScratchFlags.HasContinuations
                | false -> ScratchFlags.Default
            let scratch =
                match ignorables with
                | true -> withContinuations ||| ScratchFlags.HasDefaultIgnorables
                | false -> withContinuations
            GlyphRun.enteredSet run run.Infos props scratch

    /// <summary>Sets every glyph's mask and Unicode properties in one pass, the fused form of <see cref="setUnicodeProps"/>: a run of
    /// ASCII is set in one loop over the table, any other run through the unfolder.</summary>
    /// <param name="globalMask">The mask assigned to every glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the mask and properties set.</returns>
    let setUnicodePropsFused globalMask (run: GlyphRun) =
        match Sealed.asciiProps UnicodeProps.latinTable globalMask UnicodeProps.Continuation UnicodeProps.Ignorable (run.Infos.AsSpan()) with
        | ValueNone ->
            match Sealed.lowProps UnicodeProps.lowTable globalMask run.ScratchFlags ZwjCodepoint FractionSlashCodepoint (run.Infos.AsSpan()) with
            | ValueNone -> setUnicodePropsUnfolded globalMask run
            | ValueSome (struct (infos, props, scratch)) -> { run with Infos = infos; Props = props; ScratchFlags = scratch }
        | ValueSome (struct (infos, props, continuations, ignorables)) ->
            let withContinuations =
                match continuations with
                | true -> run.ScratchFlags ||| ScratchFlags.HasContinuations
                | false -> run.ScratchFlags
            let scratch =
                match ignorables with
                | true -> withContinuations ||| ScratchFlags.HasDefaultIgnorables
                | false -> withContinuations
            { run with Infos = infos; Props = props; ScratchFlags = scratch }
