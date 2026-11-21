namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Arabic shaper's hooks, <c>_hb_ot_shaper_arabic</c> (src/hb-ot-shaper-arabic.cc:35-805): the joining state machine
/// and the masks it sets, the features and the pauses between them, the stretching of <c>stch</c> glyphs and the reordering of the
/// modifier combining marks. The Unicode and Windows-1256 fallback shaping is <see cref="ArabicFallback"/>. The joining is public
/// because the universal shaping engine joins its Arabic-like scripts with the same machine
/// (<c>setup_masks_arabic_plan</c>, src/hb-ot-shaper-arabic.cc:393-411).</summary>
[<RequireQualifiedAccess>]
module internal Arabic =
    /// <summary>The feature <c>stch</c>, the stretching of tiles.</summary>
    [<Literal>]
    let private Stch = 0x73746368u<tag>

    /// <summary>The feature <c>isol</c>, the isolated forms.</summary>
    [<Literal>]
    let private Isol = 0x69736F6Cu<tag>

    /// <summary>The feature <c>fina</c>, the final forms.</summary>
    [<Literal>]
    let private Fina = 0x66696E61u<tag>

    /// <summary>The feature <c>fin2</c>, the final forms of a Syriac letter after an alaph.</summary>
    [<Literal>]
    let private Fin2 = 0x66696E32u<tag>

    /// <summary>The feature <c>fin3</c>, the final forms of a Syriac letter after a dalath or rish.</summary>
    [<Literal>]
    let private Fin3 = 0x66696E33u<tag>

    /// <summary>The feature <c>medi</c>, the medial forms.</summary>
    [<Literal>]
    let private Medi = 0x6D656469u<tag>

    /// <summary>The feature <c>med2</c>, the medial forms of a Syriac letter before an alaph.</summary>
    [<Literal>]
    let private Med2 = 0x6D656432u<tag>

    /// <summary>The feature <c>init</c>, the initial forms.</summary>
    [<Literal>]
    let private Init = 0x696E6974u<tag>

    /// <summary>The feature <c>mset</c>, the positioning of marks by substitution.</summary>
    [<Literal>]
    let private Mset = 0x6D736574u<tag>

    /// <summary>The action of an isolated letter, <c>ISOL</c>.</summary>
    [<Literal>]
    let private ActionIsol = 0uy

    /// <summary>The action of a final letter, <c>FINA</c>.</summary>
    [<Literal>]
    let private ActionFina = 1uy

    /// <summary>The action of a final Syriac letter after an alaph, <c>FIN2</c>.</summary>
    [<Literal>]
    let private ActionFin2 = 2uy

    /// <summary>The action of a final Syriac letter after a dalath or rish, <c>FIN3</c>.</summary>
    [<Literal>]
    let private ActionFin3 = 3uy

    /// <summary>The action of a medial letter, <c>MEDI</c>.</summary>
    [<Literal>]
    let private ActionMedi = 4uy

    /// <summary>The action of a medial Syriac letter before an alaph, <c>MED2</c>.</summary>
    [<Literal>]
    let private ActionMed2 = 5uy

    /// <summary>The action of an initial letter, <c>INIT</c>.</summary>
    [<Literal>]
    let private ActionInit = 6uy

    /// <summary>The action of a glyph that takes none, <c>NONE</c>, whose mask is zero.</summary>
    [<Literal>]
    let private ActionNone = 7uy

    /// <summary>The action of a tile of fixed width that <c>stch</c> made, <c>STCH_FIXED</c>.</summary>
    [<Literal>]
    let private ActionStchFixed = 8uy

    /// <summary>The action of a tile that <c>stch</c> made and that repeats, <c>STCH_REPEATING</c>.</summary>
    [<Literal>]
    let private ActionStchRepeating = 9uy

    /// <summary>The joining type of a letter that joins to the one before it, <c>JOINING_TYPE_R</c>.</summary>
    [<Literal>]
    let private JoiningRight = 2uy

    /// <summary>The joining type of a transparent character, <c>JOINING_TYPE_T</c>.</summary>
    [<Literal>]
    let private JoiningTransparent = 6uy

    /// <summary>The joining type the table leaves to the general category, <c>JOINING_TYPE_X</c>.</summary>
    [<Literal>]
    let private JoiningUnknown = 7uy

    /// <summary>The joining type of a character that does not join, <c>JOINING_TYPE_U</c>.</summary>
    [<Literal>]
    let private JoiningUnjoining = 0uy

    /// <summary>The number of columns of the state table, <c>NUM_STATE_MACHINE_COLS</c>.</summary>
    [<Literal>]
    let private StateColumns = 6

    /// <summary>The first state in which the previous letter may still be given an action.</summary>
    [<Literal>]
    let private FirstPossiblePrevState = 2

    /// <summary>The last state in which the previous letter may still be given an action.</summary>
    [<Literal>]
    let private LastPossiblePrevState = 5

    /// <summary>The most glyphs a group of tiles may come to, <c>STCH_MAX_GLYPHS</c>.</summary>
    [<Literal>]
    let private StchMaxGlyphs = 256

    /// <summary>The combining class of the marks below, the first class <c>reorder_marks_arabic</c> looks at.</summary>
    [<Literal>]
    let private CccAboveBelowFirst = 220

    /// <summary>The combining class of the marks above, the last class <c>reorder_marks_arabic</c> looks at.</summary>
    [<Literal>]
    let private CccAboveBelowLast = 230

    /// <summary>The modified combining class the moved marks below are given, <c>HB_MODIFIED_COMBINING_CLASS_CCC22</c> (src/hb-unicode.hh:345).</summary>
    [<Literal>]
    let private ModifiedClassBelow = 25uy

    /// <summary>The modified combining class the moved marks above are given, <c>HB_MODIFIED_COMBINING_CLASS_CCC26</c> (src/hb-unicode.hh:349).</summary>
    [<Literal>]
    let private ModifiedClassAbove = 26uy

    /// <summary>The state of the flag pass that looks at the next glyph.</summary>
    [<Literal>]
    let private FlagExamine = 0

    /// <summary>The state of the flag pass that moves on to the next glyph after a flag was set.</summary>
    [<Literal>]
    let private FlagAdvance = 1

    /// <summary>The state of the flag pass that has finished.</summary>
    [<Literal>]
    let private FlagDone = 2

    /// <summary>The glyph flag of a step that sets none.</summary>
    [<Literal>]
    let private FlagNone = 0uy

    /// <summary>The glyph flag of a step that calls <c>safe_to_insert_tatweel</c>.</summary>
    [<Literal>]
    let private FlagSafeToInsertTatweel = 1uy

    /// <summary>The glyph flag of a step that calls <c>unsafe_to_concat_from_outbuffer</c>.</summary>
    [<Literal>]
    let private FlagUnsafeToConcatFromOutbuffer = 2uy

    /// <summary>The glyph flag of a step that calls <c>unsafe_to_concat</c>.</summary>
    [<Literal>]
    let private FlagUnsafeToConcat = 3uy

    /// <summary>The features the joining actions select, in the order of the actions, <c>arabic_features</c>.</summary>
    let private joiningFeatures : ImmutableArray<Tag> =
        Sealed.ofSpan (ReadOnlySpan [| Isol; Fina; Fin2; Fin3; Medi; Med2; Init |])

    /// <summary>The joining state table, <c>arabic_state_table</c>: for each of the seven states and each of the six joining types
    /// the action for the previous letter, the action for this one and the next state.</summary>
    let private stateTable : ImmutableArray<struct (byte * byte * byte)> =
        Sealed.ofSpan (ReadOnlySpan [|
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionIsol, 1uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionIsol, 1uy); struct (ActionNone, ActionIsol, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionIsol, 1uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionFin2, 5uy); struct (ActionNone, ActionIsol, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionInit, ActionFina, 1uy); struct (ActionInit, ActionFina, 3uy); struct (ActionInit, ActionFina, 4uy); struct (ActionInit, ActionFina, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionMedi, ActionFina, 1uy); struct (ActionMedi, ActionFina, 3uy); struct (ActionMedi, ActionFina, 4uy); struct (ActionMedi, ActionFina, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionMed2, ActionIsol, 1uy); struct (ActionMed2, ActionIsol, 2uy); struct (ActionMed2, ActionFin2, 5uy); struct (ActionMed2, ActionIsol, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionIsol, ActionIsol, 1uy); struct (ActionIsol, ActionIsol, 2uy); struct (ActionIsol, ActionFin2, 5uy); struct (ActionIsol, ActionIsol, 6uy)
            struct (ActionNone, ActionNone, 0uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionIsol, 1uy); struct (ActionNone, ActionIsol, 2uy); struct (ActionNone, ActionFin3, 5uy); struct (ActionNone, ActionIsol, 6uy)
        |])

    /// <summary>Whether a feature is one of the Syriac ones, <c>FEATURE_IS_SYRIAC</c>: its last character is '2' or '3'.</summary>
    /// <param name="tag">The feature tag.</param>
    /// <returns>True for <c>fin2</c>, <c>fin3</c> and <c>med2</c>.</returns>
    let private isSyriacFeature (tag: Tag) =
        let last = uint32 tag &&& 0xFFu
        last >= 0x32u && last <= 0x33u

    /// <summary>Adds the joining features, each with a pause after it, <c>collect_features_arabic</c>'s loop.</summary>
    /// <param name="script">The script of the plan.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the features and pauses added.</returns>
    [<TailCall>]
    let rec private addJoiningFeatures (script: Script) (index: int) (builder: MapBuilder) =
        match index >= joiningFeatures.Length with
        | true -> builder
        | false ->
            let tag = joiningFeatures[index]
            let fallback = script = Scripts.Arabic && not (isSyriacFeature tag)
            let flags = match fallback with | true -> FeatureFlags.ManualZwj ||| FeatureFlags.HasFallback | false -> FeatureFlags.ManualZwj
            addJoiningFeatures script (index + 1) (builder |> MapBuilder.addFeature tag flags 1u |> MapBuilder.addGsubPause NoPause)

    /// <summary>Adds the fallback pause when the script is Arabic.</summary>
    /// <param name="script">The script of the plan.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder, with the pause when the script is Arabic.</returns>
    let private addFallbackPause (script: Script) (builder: MapBuilder) =
        match script = Scripts.Arabic with
        | true -> MapBuilder.addGsubPause ArabicFallbackShape builder
        | false -> builder

    /// <summary>Adds a plain pause unless the font has <c>rclt</c>, which HarfBuzz's issue 1573 asks for.</summary>
    /// <param name="hasRclt">Whether the chosen script and language of the font have <c>rclt</c>.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder, with the pause when there is no <c>rclt</c>.</returns>
    let private addRcltPause (hasRclt: bool) (builder: MapBuilder) =
        match hasRclt with
        | true -> builder
        | false -> MapBuilder.addGsubPause NoPause builder

    /// <summary>Adds the features and pauses of the Arabic shaper, <c>collect_features_arabic</c>: <c>stch</c> then the pause that
    /// records its glyphs, <c>ccmp</c> and <c>locl</c>, the joining features one at a time, the pause that releases the actions,
    /// <c>rlig</c> and the pause of the fallback, <c>calt</c>, and the others.</summary>
    /// <param name="script">The script of the plan, <c>plan-&gt;props.script</c>.</param>
    /// <param name="hasRclt">Whether the font has <c>rclt</c> for the chosen script and language, <c>map-&gt;has_feature</c>.</param>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features and pauses added.</returns>
    let collectFeatures (script: Script) (hasRclt: bool) (builder: MapBuilder) =
        builder
        |> MapBuilder.enableFeature Stch FeatureFlags.None 1u
        |> MapBuilder.addGsubPause ArabicRecordStch
        |> MapBuilder.enableFeature Tags.Ccmp FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Tags.Locl FeatureFlags.ManualZwj 1u
        |> MapBuilder.addGsubPause NoPause
        |> addJoiningFeatures script 0
        |> MapBuilder.addGsubPause ArabicReleaseAction
        |> MapBuilder.enableFeature Tags.Rlig (FeatureFlags.ManualZwj ||| FeatureFlags.HasFallback) 1u
        |> addFallbackPause script
        |> MapBuilder.enableFeature Tags.Calt FeatureFlags.ManualZwj 1u
        |> addRcltPause hasRclt
        |> MapBuilder.enableFeature Tags.Liga FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Tags.Clig FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Mset FeatureFlags.ManualZwj 1u

    /// <summary>Whether every joining feature that is not Syriac needs the fallback, the loop of <c>data_create_arabic</c>.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="index">The next feature.</param>
    /// <returns>True when each one is Syriac or has no lookup in the font.</returns>
    [<TailCall>]
    let rec private allNeedFallback (map: OtMap) (index: int) =
        match index >= joiningFeatures.Length with
        | true -> true
        | false ->
            let tag = joiningFeatures[index]
            match isSyriacFeature tag || MapBuilder.needsFallback map tag with
            | true -> allNeedFallback map (index + 1)
            | false -> false

    /// <summary>Reads the masks of the joining features and whether the plan stretches or falls back from the compiled map,
    /// <c>data_create_arabic</c>, which the Arabic shaper and the universal shaping engine of a script that joins both call.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="map">The compiled map.</param>
    /// <returns>The plan.</returns>
    let createPlan (mask1: Tag -> uint32) (props: SegmentProperties) (map: OtMap) =
        let masks =
            Sealed.init (int ActionNone + 1) (fun action ->
                match action < joiningFeatures.Length with
                | true -> mask1 joiningFeatures[action]
                | false -> 0u)
        { MaskArray = masks
          HasStch = mask1 Stch <> 0u
          DoFallback = props.Script = Scripts.Arabic && allNeedFallback map 0 }

    /// <summary>The plan data of the Arabic shaper, <see cref="createPlan"/>.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="map">The compiled map.</param>
    /// <returns>The plan data.</returns>
    let createData (mask1: Tag -> uint32) (props: SegmentProperties) (map: OtMap) = ArabicData (createPlan mask1 props map)

    /// <summary>Whether a general category is one of the word categories, <c>HB_ARABIC_GENERAL_CATEGORY_IS_WORD</c>.</summary>
    /// <param name="category">The general category.</param>
    /// <returns>True for a letter other than cased, a mark, a number or a symbol, and for unassigned and private use.</returns>
    let private isWordCategory (category: GeneralCategory) =
        match category with
        | Unassigned | PrivateUse | ModifierLetter | OtherLetter | SpacingMark | EnclosingMark | NonSpacingMark
        | DecimalNumber | LetterNumber | OtherNumber | CurrencySymbol | ModifierSymbol | MathSymbol | OtherSymbol -> true
        | Control | Format | Surrogate | LowercaseLetter | TitlecaseLetter | UppercaseLetter | ConnectPunctuation
        | DashPunctuation | ClosePunctuation | FinalPunctuation | InitialPunctuation | OtherPunctuation | OpenPunctuation
        | LineSeparator | ParagraphSeparator | SpaceSeparator -> false

    /// <summary>Whether a general category makes a character of unknown joining type transparent, <c>get_joining_type</c>.</summary>
    /// <param name="category">The general category.</param>
    /// <returns>True for a non-spacing or enclosing mark or a format character.</returns>
    let private isTransparentCategory (category: GeneralCategory) =
        match category with
        | NonSpacingMark | EnclosingMark | Format -> true
        | Control | Unassigned | PrivateUse | Surrogate | LowercaseLetter | ModifierLetter | OtherLetter | TitlecaseLetter
        | UppercaseLetter | SpacingMark | DecimalNumber | LetterNumber | OtherNumber | ConnectPunctuation | DashPunctuation
        | ClosePunctuation | FinalPunctuation | InitialPunctuation | OtherPunctuation | OpenPunctuation | CurrencySymbol
        | ModifierSymbol | MathSymbol | OtherSymbol | LineSeparator | ParagraphSeparator | SpaceSeparator -> false

    /// <summary>The joining type of a character, <c>get_joining_type</c>: the table's, and for a character the table leaves open
    /// transparent or not by its general category.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="category">The general category of the character.</param>
    /// <returns>The joining type.</returns>
    let private joiningType (u: uint32) (category: GeneralCategory) =
        let tableType = ArabicTable.joiningType u
        match tableType = JoiningUnknown with
        | false -> tableType
        | true ->
            match isTransparentCategory category with
            | true -> JoiningTransparent
            | false -> JoiningUnjoining

    /// <summary>The state of the joining machine: the next index, the previous letter that took part, the state of the table and
    /// whether the context after the text has already been looked at.</summary>
    [<Struct>]
    type private JoinState =
        { Index: int
          Prev: int
          Machine: int
          Closed: bool }

    /// <summary>What the joining machine does at one glyph: the action of the glyph, the action it gives the previous letter, that
    /// letter, and the glyph flag to set over a range.</summary>
    [<Struct>]
    type private JoinStep =
        { Skip: bool
          Curr: byte
          PrevAction: byte
          Prev: int
          Flag: byte
          FlagStart: int
          FlagEnd: int }

    /// <summary>The step of a transparent glyph, or of a context after the first that counted.</summary>
    let private skipped : JoinStep =
        { Skip = true; Curr = ActionNone; PrevAction = ActionNone; Prev = -1; Flag = FlagNone; FlagStart = 0; FlagEnd = 0 }

    /// <summary>The glyph flag of a step, <c>arabic_joining</c>'s <c>safe_to_insert_tatweel</c> and <c>unsafe_to_concat</c> calls.</summary>
    /// <param name="overrides">Whether the step sets an action on the previous letter.</param>
    /// <param name="state">The state before the step.</param>
    /// <param name="thisType">The joining type of the glyph.</param>
    /// <param name="end">The end of the flagged range.</param>
    /// <returns>The flag, its start and its end.</returns>
    let private flagOf (overrides: bool) (state: JoinState) (thisType: int) (``end``: int) =
        match overrides with
        | true -> struct (FlagSafeToInsertTatweel, state.Prev, ``end``)
        | false ->
            match state.Prev < 0 with
            | true ->
                match thisType >= int JoiningRight with
                | true -> struct (FlagUnsafeToConcatFromOutbuffer, 0, ``end``)
                | false -> struct (FlagNone, 0, 0)
            | false ->
                match thisType >= int JoiningRight || (FirstPossiblePrevState <= state.Machine && state.Machine <= LastPossiblePrevState) with
                | true -> struct (FlagUnsafeToConcat, state.Prev, ``end``)
                | false -> struct (FlagNone, 0, 0)

    /// <summary>The glyph flag of the first context character after the text that is not transparent, the last loop of
    /// <c>arabic_joining</c>.</summary>
    /// <param name="overrides">Whether the character sets an action on the last letter of the text.</param>
    /// <param name="state">The state before the character. When the context before the text put the machine in a state that
    /// expects a letter and the text has none, the range starts at <c>UINT_MAX</c> upstream, which flags nothing.</param>
    /// <param name="count">The number of glyphs, where the flagged range ends.</param>
    /// <returns>The flag, its start and its end.</returns>
    let private contextFlagOf (overrides: bool) (state: JoinState) (count: int) =
        match overrides with
        | true -> struct (FlagSafeToInsertTatweel, state.Prev, count)
        | false ->
            match FirstPossiblePrevState <= state.Machine && state.Machine <= LastPossiblePrevState && state.Prev >= 0 with
            | true -> struct (FlagUnsafeToConcat, state.Prev, count)
            | false -> struct (FlagNone, 0, 0)

    /// <summary>The joining machine as a folder over the joining types of the glyphs followed by those of the context after them.</summary>
    [<Struct>]
    type private Joiner =
        val Count: int

        /// <summary>Creates the folder.</summary>
        /// <param name="count">The number of glyphs, the types after which are those of the context after the text.</param>
        new (count) = { Count = count }

        interface ISpanUnfolder<byte, JoinState, JoinStep> with
            member this.Next (types, state) =
                let thisType = int types[state.Index]
                match state.Closed || thisType = int JoiningTransparent with
                | true -> struct (skipped, { state with Index = state.Index + 1 })
                | false ->
                    let struct (prevAction, currAction, next) = stateTable[state.Machine * StateColumns + thisType]
                    let overrides = prevAction <> ActionNone && state.Prev >= 0
                    match state.Index >= this.Count with
                    | true ->
                        let struct (flag, flagStart, flagEnd) = contextFlagOf overrides state this.Count
                        let step = { Skip = false; Curr = ActionNone; PrevAction = (match overrides with | true -> prevAction | false -> ActionNone); Prev = state.Prev; Flag = flag; FlagStart = flagStart; FlagEnd = flagEnd }
                        struct (step, { state with Index = state.Index + 1; Closed = true })
                    | false ->
                        let struct (flag, flagStart, flagEnd) = flagOf overrides state thisType (state.Index + 1)
                        let step = { Skip = false; Curr = currAction; PrevAction = (match overrides with | true -> prevAction | false -> ActionNone); Prev = state.Prev; Flag = flag; FlagStart = flagStart; FlagEnd = flagEnd }
                        struct (step, { Index = state.Index + 1; Prev = state.Index; Machine = int next; Closed = false })

    /// <summary>The state of the table after the context before the text, the first part of <c>arabic_joining</c>: the first
    /// character that is not transparent moves the machine from state zero.</summary>
    /// <param name="category">The general category of a context character.</param>
    /// <param name="before">The context before the text, nearest first.</param>
    /// <param name="index">The next context character.</param>
    /// <returns>The state.</returns>
    [<TailCall>]
    let rec private preContextState (category: uint32 -> GeneralCategory) (before: ImmutableArray<uint32>) (index: int) =
        match index >= before.Length with
        | true -> 0
        | false ->
            let u = before[index]
            let thisType = joiningType u (category u)
            match thisType = JoiningTransparent with
            | true -> preContextState category before (index + 1)
            | false ->
                let struct (_, _, next) = stateTable[int thisType]
                int next

    /// <summary>The index of the next step that is not skipped.</summary>
    /// <param name="steps">The steps.</param>
    /// <param name="index">The next step to look at.</param>
    /// <returns>The index, or the length when there is none.</returns>
    [<TailCall>]
    let rec private nextActive (steps: ImmutableArray<JoinStep>) (index: int) =
        match index >= steps.Length with
        | true -> steps.Length
        | false ->
            match steps[index].Skip with
            | true -> nextActive steps (index + 1)
            | false -> index

    /// <summary>The action of a glyph once the next letter has given it its own, <c>info[prev].arabic_shaping_action() =
    /// entry-&gt;prev_action</c>.</summary>
    /// <param name="steps">The steps of the glyphs and the context after them.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>The action.</returns>
    let private finalAction (steps: ImmutableArray<JoinStep>) (index: int) =
        let following = nextActive steps (index + 1)
        match following < steps.Length && steps[following].Prev = index && steps[following].PrevAction <> ActionNone with
        | true -> steps[following].PrevAction
        | false -> steps[index].Curr

    /// <summary>The folder that gives a Mongolian variation selector the action of the letter before it,
    /// <c>mongolian_variation_selectors</c>.</summary>
    [<Struct>]
    type private MongolianFolder =
        val Actions: ImmutableArray<byte>

        /// <summary>Creates the folder.</summary>
        /// <param name="actions">The actions the joining gave.</param>
        new (actions) = { Actions = actions }

        interface ISpanUnfolder<GlyphInfo, struct (int * byte), byte> with
            member this.Next (infos, state) =
                let struct (index, previous) = state
                let u = infos[index].Codepoint
                let selector = (u >= 0x180Bu && u <= 0x180Du) || u = 0x180Fu
                let action = match index > 0 && selector with | true -> previous | false -> this.Actions[index]
                struct (action, struct (index + 1, action))

    /// <summary>The edit of a glyph flag a step decided on.</summary>
    /// <param name="step">The step.</param>
    /// <returns>The edit, none when the step sets no flag.</returns>
    let private flagEdit (step: JoinStep) =
        match step.Flag with
        | FlagSafeToInsertTatweel -> ValueSome (RunEdit.SafeToInsertTatweel (step.FlagStart, step.FlagEnd))
        | FlagUnsafeToConcatFromOutbuffer -> ValueSome (RunEdit.UnsafeToConcatFromOutbuffer (step.FlagStart, step.FlagEnd))
        | FlagUnsafeToConcat -> ValueSome (RunEdit.UnsafeToConcat (step.FlagStart, step.FlagEnd))
        | _ -> ValueNone

    /// <summary>The edit of the glyph flag of the context after the text, if its step sets one.</summary>
    /// <param name="steps">The steps of the glyphs and of the context after them.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <returns>The edit, none when the context sets no flag.</returns>
    let private tailFlagEdit (steps: ImmutableArray<JoinStep>) (count: int) =
        let tail = nextActive steps count
        match tail < steps.Length with
        | true -> flagEdit steps[tail]
        | false -> ValueNone

    /// <summary>Looks at the glyph the flag pass has reached: its flag edit, or at the end the flag of the context after the text.</summary>
    /// <param name="steps">The steps of the glyphs and of the context after them.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <param name="index">The glyph the pass has reached.</param>
    /// <returns>The edit and the next state of the pass.</returns>
    let private examineFlag (steps: ImmutableArray<JoinStep>) (count: int) (index: int) =
        match index >= count with
        | true ->
            match tailFlagEdit steps count with
            | ValueSome edit -> struct (edit, FlagDone)
            | ValueNone -> struct (RunEdit.Finish, FlagDone)
        | false ->
            match flagEdit steps[index] with
            | ValueSome edit -> struct (edit, FlagAdvance)
            | ValueNone -> struct (RunEdit.NextGlyph, FlagExamine)

    /// <summary>The pass that applies the glyph flags the joining machine decided on, at each glyph and, after the last, the
    /// flag of the context after the text.</summary>
    [<Struct>]
    type private FlagPass =
        val Steps: ImmutableArray<JoinStep>
        val Count: int

        /// <summary>Creates the pass.</summary>
        /// <param name="steps">The steps of the glyphs and of the context after them.</param>
        /// <param name="count">The number of glyphs.</param>
        new (steps, count) = { Steps = steps; Count = count }

        interface IRunPass<int> with
            member this.Step (cursor, state) =
                match state with
                | FlagDone -> struct (RunEdit.Finish, FlagDone)
                | FlagAdvance -> struct (RunEdit.NextGlyph, FlagExamine)
                | _ -> examineFlag this.Steps this.Count cursor.Index

    /// <summary>Whether any step carries a flag.</summary>
    /// <param name="steps">The steps.</param>
    /// <param name="index">The next step.</param>
    /// <returns>True when a flag has to be set.</returns>
    [<TailCall>]
    let rec private anyFlag (steps: ImmutableArray<JoinStep>) (index: int) =
        match index >= steps.Length with
        | true -> false
        | false ->
            match steps[index].Flag <> FlagNone with
            | true -> true
            | false -> anyFlag steps (index + 1)

    /// <summary>Gives every glyph its shaping action and its mask, <c>setup_masks_arabic_plan</c>: the joining machine over the
    /// text and its context, the Mongolian variation selectors, and the mask of the feature each action selects.</summary>
    /// <param name="category">The general category of a context character.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="script">The script of the plan.</param>
    /// <param name="run">The run, normalised, with its Unicode properties set.</param>
    /// <returns>The run with the actions in the auxiliary byte, the masks and the glyph flags set.</returns>
    let setupMasksUsing (category: uint32 -> GeneralCategory) (data: ArabicPlan) (script: Script) (run: GlyphRun) =
        let count = run.Infos.Length
        let own = Sealed.init count (fun index -> joiningType run.Infos[index].Codepoint (UnicodeProps.generalCategory run.Props[index]))
        let after = Sealed.map (fun u -> joiningType u (category u)) (run.ContextAfter.AsSpan())
        let types = Sealed.concat own after
        let seed = { Index = 0; Prev = -1; Machine = preContextState category run.ContextBefore 0; Closed = false }
        let steps = Sealed.unfoldSpan (types.AsSpan()) types.Length seed (Joiner(count))
        let joined = Sealed.init count (fun index -> finalAction steps index)
        let actions =
            match script = Scripts.Mongolian with
            | true -> Sealed.unfoldSpan (run.Infos.AsSpan()) count (struct (0, ActionNone)) (MongolianFolder(joined))
            | false -> joined
        let infos = Sealed.mapi (fun index (info: GlyphInfo) -> { info with Mask = info.Mask ||| data.MaskArray[int actions[index]] }) (run.Infos.AsSpan())
        let props = Sealed.mapi (fun index (props: GlyphProps) -> { props with ShaperAuxiliary = actions[index] }) (run.Props.AsSpan())
        let acted = { run with Infos = infos; Props = props }
        match anyFlag steps 0 with
        | false -> acted
        | true -> GlyphRun.runPass (FlagPass(steps, count)) 0 acted

    /// <summary>Gives every glyph its shaping action and its mask as <see cref="setupMasksUsing"/> does, with the general categories of
    /// the default Unicode functions for the context.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="script">The script of the plan.</param>
    /// <param name="run">The run, normalised, with its Unicode properties set.</param>
    /// <returns>The run with the actions in the auxiliary byte, the masks and the glyph flags set.</returns>
    let setupMasksPlan (data: ArabicPlan) (script: Script) (run: GlyphRun) = setupMasksUsing Unicode.generalCategory data script run

    /// <summary>Runs <see cref="setupMasksPlan"/> for the plan, <c>setup_masks_arabic</c>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="script">The script of the plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the masks set.</returns>
    let setupMasks (data: ArabicPlan) (script: Script) (run: GlyphRun) = setupMasksPlan data script run

    /// <summary>The joining types, the state before the text and the state table of the Arabic shaper, as the in-place joining pass reads them.</summary>
    [<Struct>]
    type private OwnedJoining =
        val Infos: ImmutableArray<GlyphInfo>
        val Props: ImmutableArray<GlyphProps>
        val After: ImmutableArray<uint32>
        val Before: int

        /// <summary>Creates the source.</summary>
        /// <param name="infos">The glyphs.</param>
        /// <param name="props">The properties of the glyphs.</param>
        /// <param name="after">The context after the text.</param>
        /// <param name="before">The state the context before the text leaves.</param>
        new (infos, props, after, before) = { Infos = infos; Props = props; After = after; Before = before }

        interface IArabicJoining with
            member this.Transparent = JoiningTransparent
            member this.Right = JoiningRight
            member this.NoAction = ActionNone
            member this.FirstPossiblePrevState = FirstPossiblePrevState
            member this.LastPossiblePrevState = LastPossiblePrevState
            member this.ContextCount = this.After.Length
            member this.PreContextState = this.Before
            member this.OwnType index = joiningType this.Infos[index].Codepoint (UnicodeProps.generalCategory this.Props[index])
            member this.ContextType index = joiningType this.After[index] (Unicode.generalCategory this.After[index])
            member this.Entry (state, thisType) = stateTable[state * StateColumns + int thisType]

    /// <summary>Gives every glyph of a working set its shaping action and its mask as <see cref="setupMasksPlan"/> does, in one pass over the
    /// set's own arrays with the joining state in locals and no array in between, for a set whose arrays the caller made in this call and
    /// shared with no one.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="script">The script of the plan.</param>
    /// <param name="set">The working set, normalised, with its Unicode properties set.</param>
    /// <returns>The set with the actions in the auxiliary byte, the masks and the glyph flags set.</returns>
    let setupMasksSet (data: ArabicPlan) (script: Script) (set: WorkingSet) =
        let joining = OwnedJoining(set.Infos, set.Props, set.ContextAfter, preContextState Unicode.generalCategory set.ContextBefore 0)
        let struct (maxOps, successful) = ArabicOwned.join joining set.Infos set.Props set.ClusterLevel set.Flags set.MaxOps set.Successful
        ArabicOwned.maskActions set.Infos set.Props data.MaskArray (script = Scripts.Mongolian)
        { set with MaxOps = maxOps; Successful = successful }

    /// <summary>Whether a glyph in a span was multiplied.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when some glyph from the index on was multiplied.</returns>
    [<TailCall>]
    let rec private anyMultiplied (props: ReadOnlySpan<GlyphProps>) index =
        match index >= props.Length with
        | true -> false
        | false ->
            match GlyphPropsBits.multiplied props[index] with
            | true -> true
            | false -> anyMultiplied props (index + 1)

    /// <summary>Marks the glyphs a <c>stch</c> lookup multiplied, <c>record_stch</c>: the odd components are the tiles that repeat,
    /// the even ones the tiles of fixed width.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, just after the <c>stch</c> lookups.</param>
    /// <returns>The run with the actions recorded and the scratch flag set when any glyph was multiplied.</returns>
    let recordStch (data: ArabicPlan) (run: GlyphRun) =
        match data.HasStch && anyMultiplied (run.Props.AsSpan()) 0 with
        | false -> run
        | true ->
            let props =
                Sealed.map (fun (props: GlyphProps) ->
                    match GlyphPropsBits.multiplied props with
                    | false -> props
                    | true ->
                        let action = match int (GlyphPropsBits.ligComp props) % 2 with | 1 -> ActionStchRepeating | _ -> ActionStchFixed
                        { props with ShaperAuxiliary = action }) (run.Props.AsSpan())
            { run with Props = props; ScratchFlags = run.ScratchFlags ||| ScratchFlags.Shaper0 }

    /// <summary>Whether a glyph is one a <c>stch</c> lookup multiplied and <see cref="recordStch"/> recorded.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True for a fixed or a repeating tile.</returns>
    let private isStchTile (props: GlyphProps) =
        props.ShaperAuxiliary >= ActionStchFixed && props.ShaperAuxiliary <= ActionStchRepeating

    /// <summary>Saturating subtraction of two positions, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The difference, clamped to the position range.</returns>
    let private saturateSub (a: int) (b: int) =
        int (Math.Clamp (int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Saturating addition of two positions, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first position.</param>
    /// <param name="b">The second position.</param>
    /// <returns>The sum, clamped to the position range.</returns>
    let private saturateAdd (a: int) (b: int) =
        int (Math.Clamp (int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>One row of a run: the glyph, its properties and its position.</summary>
    [<Struct>]
    type private StchRow =
        { Info: GlyphInfo
          Props: GlyphProps
          Position: GlyphPosition }

    /// <summary>The stretching of one word's tiles, the numbers <c>apply_stch</c> works out for a group of tiles.</summary>
    [<Struct>]
    type private StchGroup =
        { Context: int
          Start: int
          End: int
          Copies: int
          Overlap: int
          Remaining: int }

    /// <summary>Sums the widths of the tiles of a group, the first loop of <c>apply_stch</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next tile, counting down.</param>
    /// <param name="start">The first tile of the group.</param>
    /// <param name="fixedWidth">The width of the fixed tiles so far.</param>
    /// <param name="repeatingWidth">The width of the repeating tiles so far.</param>
    /// <param name="fixedCount">The fixed tiles so far.</param>
    /// <param name="repeatingCount">The repeating tiles so far.</param>
    /// <returns>The widths and counts of the fixed and repeating tiles.</returns>
    [<TailCall>]
    let rec private sumTiles (font: Font) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) index start fixedWidth repeatingWidth fixedCount repeatingCount =
        match index < start with
        | true -> struct (fixedWidth, repeatingWidth, fixedCount, repeatingCount)
        | false ->
            let width = Font.hAdvance font infos[index].Codepoint
            match props[index].ShaperAuxiliary = ActionStchFixed with
            | true -> sumTiles font infos props (index - 1) start (saturateAdd fixedWidth width) repeatingWidth (fixedCount + 1) repeatingCount
            | false -> sumTiles font infos props (index - 1) start fixedWidth (saturateAdd repeatingWidth width) fixedCount (repeatingCount + 1)

    /// <summary>Finds the start of a group of tiles, counting down.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first index not yet known to be a tile of the group, exclusive.</param>
    /// <returns>The index of the first tile.</returns>
    [<TailCall>]
    let rec private groupStart (props: ImmutableArray<GlyphProps>) index =
        match index > 0 && isStchTile props[index - 1] with
        | true -> groupStart props (index - 1)
        | false -> index

    /// <summary>Walks the word before a group of tiles and adds up its advances, the second loop of <c>apply_stch</c>: it stops at
    /// another tile and at anything that is not a default ignorable or a word character.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="context">The first glyph of the word so far.</param>
    /// <param name="total">The width so far.</param>
    /// <returns>The start of the word and its width.</returns>
    [<TailCall>]
    let rec private wordBefore (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) context total =
        match context > 0 && not (isStchTile props[context - 1]) && (UnicodeProps.isDefaultIgnorable props[context - 1] || isWordCategory (UnicodeProps.generalCategory props[context - 1])) with
        | false -> struct (context, total)
        | true -> wordBefore props positions (context - 1) (saturateAdd total positions[context - 1].XAdvance)

    /// <summary>Works out how many copies of the repeating tiles fill the word and how much they overlap, <c>apply_stch</c>.</summary>
    /// <param name="sign">One for a font with a positive scale, minus one for a negative one.</param>
    /// <param name="total">The width of the word.</param>
    /// <param name="fixedWidth">The width of the fixed tiles.</param>
    /// <param name="repeatingWidth">The width of the repeating tiles.</param>
    /// <param name="fixedCount">The number of fixed tiles.</param>
    /// <param name="repeatingCount">The number of repeating tiles.</param>
    /// <returns>The copies of each repeating tile, the overlap of the copies and the width left to centre.</returns>
    let private fitTiles (sign: int) (total: int) (fixedWidth: int) (repeatingWidth: int) (fixedCount: int) (repeatingCount: int) =
        let remainingSigned = int64 total - int64 fixedWidth
        let repeatingSigned = int64 repeatingWidth
        let remainingSigned = match sign < 0 with | true -> -remainingSigned | false -> remainingSigned
        let repeatingSigned = match sign < 0 with | true -> -repeatingSigned | false -> repeatingSigned
        let remaining = saturateSub total fixedWidth
        let copies = match remainingSigned > repeatingSigned && repeatingSigned > 0L with | true -> uint32 (remainingSigned / repeatingSigned - 1L) | false -> 0u
        let shortfall = remainingSigned - repeatingSigned * int64 (copies + 1u)
        let struct (copies, overlap, remaining) =
            match shortfall > 0L && repeatingCount > 0 with
            | false -> struct (copies, 0, remaining)
            | true ->
                let more = copies + 1u
                let excess = int64 (more + 1u) * repeatingSigned - remainingSigned
                match excess > 0L with
                | false -> struct (more, 0, remaining)
                | true -> struct (more, int (Math.Clamp (excess / (int64 more * int64 repeatingCount), int64 Int32.MinValue, int64 Int32.MaxValue)), 0)
        let maxCopies =
            match repeatingCount > 0 && fixedCount + repeatingCount < StchMaxGlyphs with
            | true -> uint32 ((StchMaxGlyphs - fixedCount - repeatingCount) / repeatingCount)
            | false -> 0u
        struct (int (min copies maxCopies), overlap, remaining)

    /// <summary>Finds the groups of tiles of a run, from the end, the measuring step of <c>apply_stch</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="sign">The sign of the font's scale.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The end of the part of the run not yet looked at.</param>
    /// <param name="groups">The groups found so far, the last in the run first.</param>
    /// <returns>The groups, the first in the run first.</returns>
    [<TailCall>]
    let rec private findGroups (font: Font) (sign: int) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) index (groups: ImmutableArray<StchGroup>) =
        match index = 0 with
        | true -> groups
        | false ->
            match isStchTile props[index - 1] with
            | false -> findGroups font sign infos props positions (index - 1) groups
            | true ->
                let start = groupStart props index
                let struct (fixedWidth, repeatingWidth, fixedCount, repeatingCount) = sumTiles font infos props (index - 1) start 0 0 0 0
                let struct (context, total) = wordBefore props positions start 0
                let struct (copies, overlap, remaining) = fitTiles sign total fixedWidth repeatingWidth fixedCount repeatingCount
                let group = { Context = context; Start = start; End = index; Copies = copies; Overlap = overlap; Remaining = remaining }
                findGroups font sign infos props positions start (Sealed.concat (Sealed.ofSpan (ReadOnlySpan [| group |])) groups)

    /// <summary>The pass that marks the word of each group unsafe to break.</summary>
    [<Struct>]
    type private BreakPass =
        val Groups: ImmutableArray<StchGroup>

        /// <summary>Creates the pass.</summary>
        /// <param name="groups">The groups.</param>
        new (groups) = { Groups = groups }

        interface IRunPass<int> with
            member this.Step (_, state) =
                match state >= this.Groups.Length with
                | true -> struct (RunEdit.Finish, state)
                | false -> struct (RunEdit.UnsafeToBreak (this.Groups[state].Context, this.Groups[state].End), state + 1)

    /// <summary>Lays the copies of one tile out, <c>apply_stch</c>'s inner loop: the tile and its copies each get an offset,
    /// and the tile's own advance is zero. The copies come out in the order they stand in the run, the last copy first.</summary>
    /// <param name="rtl">Whether the run is right to left.</param>
    /// <param name="group">The group.</param>
    /// <param name="width">The advance of the tile.</param>
    /// <param name="repeat">The number of copies, the tile included.</param>
    /// <param name="n">The next copy.</param>
    /// <param name="offset">The offset so far.</param>
    /// <param name="row">The tile.</param>
    /// <param name="copies">The copies so far, the last copy first.</param>
    /// <returns>The copies and the offset after them.</returns>
    [<TailCall>]
    let rec private layOut (rtl: bool) (group: StchGroup) (width: int) (repeat: int) (n: int) (offset: int) (row: StchRow) (copies: ImmutableArray<StchRow>) =
        match n >= repeat with
        | true -> struct (copies, offset)
        | false ->
            let before =
                match rtl with
                | true -> (match n > 0 with | true -> saturateAdd (saturateSub offset width) group.Overlap | false -> saturateSub offset width)
                | false -> offset
            let placed = { row with Position = { row.Position with XAdvance = 0; XOffset = before } }
            let after =
                match rtl with
                | true -> before
                | false -> (match n > 0 with | true -> saturateSub (saturateAdd before width) group.Overlap | false -> saturateAdd before width)
            layOut rtl group width repeat (n + 1) after row (Sealed.concat (Sealed.ofSpan (ReadOnlySpan [| placed |])) copies)

    /// <summary>Lays the tiles of a group out, from the last tile to the first, the second step of <c>apply_stch</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="rtl">Whether the run is right to left.</param>
    /// <param name="rows">The rows of the run.</param>
    /// <param name="group">The group.</param>
    /// <param name="index">The next tile, counting down.</param>
    /// <param name="offset">The offset so far.</param>
    /// <param name="output">The rows laid out so far, the first tile first.</param>
    /// <returns>The rows of the group.</returns>
    [<TailCall>]
    let rec private layOutTiles (font: Font) (rtl: bool) (rows: ImmutableArray<StchRow>) (group: StchGroup) index (offset: int) (output: ImmutableArray<StchRow>) =
        match index < group.Start with
        | true -> output
        | false ->
            let row = rows[index]
            let width = Font.hAdvance font row.Info.Codepoint
            let repeat = match row.Props.ShaperAuxiliary = ActionStchRepeating with | true -> 1 + group.Copies | false -> 1
            let struct (copies, next) = layOut rtl group width repeat 0 offset rows[index] ImmutableArray.Empty
            layOutTiles font rtl rows group (index - 1) next (Sealed.concat copies output)

    /// <summary>Cuts the run, copying every row and laying the tiles out where a group stands, from the end of the run.</summary>
    /// <param name="font">The font.</param>
    /// <param name="rtl">Whether the run is right to left.</param>
    /// <param name="rows">The rows of the run.</param>
    /// <param name="groups">The groups, the first in the run first.</param>
    /// <param name="index">The end of the part of the run not yet cut.</param>
    /// <param name="group">The next group, counting down.</param>
    /// <param name="output">The rows cut so far.</param>
    /// <returns>The rows of the run with the tiles stretched.</returns>
    [<TailCall>]
    let rec private cut (font: Font) (rtl: bool) (rows: ImmutableArray<StchRow>) (groups: ImmutableArray<StchGroup>) index (group: int) (output: ImmutableArray<StchRow>) =
        match index = 0 with
        | true -> output
        | false ->
            match group >= 0 && groups[group].End = index with
            | false -> cut font rtl rows groups (index - 1) group (Sealed.concat (Sealed.ofSpan (ReadOnlySpan [| rows[index - 1] |])) output)
            | true ->
                let current = groups[group]
                let laid = layOutTiles font rtl rows current (index - 1) (current.Remaining / 2) ImmutableArray.Empty
                cut font rtl rows groups current.Start (group - 1) (Sealed.concat laid output)

    /// <summary>Stretches the tiles of a run that stands in the direction of its text, <c>apply_stch</c>'s two steps.</summary>
    /// <param name="font">The font.</param>
    /// <param name="rtl">Whether the run is right to left.</param>
    /// <param name="run">The run, with positions.</param>
    /// <returns>The run with its tiles stretched.</returns>
    let private stretch (font: Font) (rtl: bool) (run: GlyphRun) =
        let sign = match font.XScale < 0 with | true -> -1 | false -> 1
        let groups = findGroups font sign run.Infos run.Props run.Positions run.Infos.Length ImmutableArray.Empty
        match groups.IsEmpty with
        | true -> run
        | false ->
            let flagged = GlyphRun.runPass (BreakPass(groups)) 0 run
            let rows = Sealed.init flagged.Infos.Length (fun index -> { Info = flagged.Infos[index]; Props = flagged.Props[index]; Position = flagged.Positions[index] })
            let cutRows = cut font rtl rows groups rows.Length (groups.Length - 1) ImmutableArray.Empty
            { flagged with
                Infos = Sealed.map (fun (row: StchRow) -> row.Info) (cutRows.AsSpan())
                Props = Sealed.map (fun (row: StchRow) -> row.Props) (cutRows.AsSpan())
                Positions = Sealed.map (fun (row: StchRow) -> row.Position) (cutRows.AsSpan()) }

    /// <summary>Stretches the tiles <c>stch</c> multiplied, <c>postprocess_glyphs_arabic</c> and <c>apply_stch</c>: the tiles of
    /// each group fill the word they stand in, the repeating ones copied as often as fits.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, substituted and positioned.</param>
    /// <returns>The run with the tiles stretched.</returns>
    let postprocess (font: Font) (run: GlyphRun) =
        match (run.ScratchFlags &&& ScratchFlags.Shaper0) = ScratchFlags.Default with
        | true -> run
        | false ->
            let rtl = run.SegmentProperties.Direction = Direction.Rtl
            match rtl with
            | true -> stretch font true run
            | false -> GlyphRun.reverse (stretch font false (GlyphRun.reverse run))

    /// <summary>The marks <c>modifier_combining_marks</c> lists, <c>https://www.unicode.org/reports/tr53/</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for a modifier combining mark.</returns>
    let private isModifierCombiningMark (u: uint32) =
        match u with
        | 0x0654u | 0x0655u | 0x0658u | 0x06DCu | 0x06E3u | 0x06E7u | 0x06E8u | 0x08CAu | 0x08CBu | 0x08CDu | 0x08CEu | 0x08CFu | 0x08D3u | 0x08F3u -> true
        | _ -> false

    /// <summary>The pass that applies a list of edits in order.</summary>
    [<Struct>]
    type private EditsPass =
        val Edits: ImmutableArray<RunEdit>

        /// <summary>Creates the pass.</summary>
        /// <param name="edits">The edits.</param>
        new (edits) = { Edits = edits }

        interface IRunPass<int> with
            member this.Step (_, state) =
                match state >= this.Edits.Length with
                | true -> struct (RunEdit.Finish, state)
                | false -> struct (this.Edits[state], state + 1)

    /// <summary>Advances over the marks whose class is below the one looked for.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <param name="cc">The class.</param>
    /// <param name="index">The next mark.</param>
    /// <returns>The first mark whose class is the one looked for or more.</returns>
    [<TailCall>]
    let rec private skipBelow (props: ReadOnlySpan<GlyphProps>) finish (cc: int) index =
        match index < finish && int (UnicodeProps.modifiedCombiningClass props[index]) < cc with
        | true -> skipBelow props finish cc (index + 1)
        | false -> index

    /// <summary>Advances over the modifier combining marks of a class.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <param name="cc">The class.</param>
    /// <param name="index">The next mark.</param>
    /// <returns>The end of the modifier combining marks of the class that start at the index.</returns>
    [<TailCall>]
    let rec private modifierMarksEnd (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) finish (cc: int) index =
        match index < finish && int (UnicodeProps.modifiedCombiningClass props[index]) = cc && isModifierCombiningMark infos[index].Codepoint with
        | true -> modifierMarksEnd props infos finish cc (index + 1)
        | false -> index

    /// <summary>The edits that put the marks renumbered after their shift, one for each mark moved.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="newClass">The class they are given.</param>
    /// <param name="start">Where the first of them now stands.</param>
    /// <param name="first">Where the first of them stood.</param>
    /// <param name="last">The end of the marks, where they stood.</param>
    /// <param name="edits">The edits so far.</param>
    /// <returns>The edits with the renumbering added.</returns>
    [<TailCall>]
    let rec private renumber (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) (newClass: byte) start first last (edits: ImmutableArray<RunEdit>) =
        match first >= last with
        | true -> edits
        | false ->
            let edit = RunEdit.SetInputGlyph (start, infos[first].Codepoint, UnicodeProps.setModifiedCombiningClass newClass props[first])
            renumber props infos newClass (start + 1) (first + 1) last (Sealed.appendOne (edits.AsSpan()) edit)

    /// <summary>The edits of one shift of modifier combining marks to the front: the clusters merged, the marks exchanged with those
    /// before them and renumbered.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="cc">The class of the marks.</param>
    /// <param name="start">Where the marks go.</param>
    /// <param name="at">Where the marks are.</param>
    /// <param name="upTo">The end of the marks.</param>
    /// <param name="edits">The edits so far.</param>
    /// <returns>The edits with the shift added.</returns>
    let private shiftEdits (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) (cc: int) start at upTo (edits: ImmutableArray<RunEdit>) =
        let merged = Sealed.appendOne (edits.AsSpan()) (RunEdit.MergeClusters (start, upTo))
        let moved =
            match at = start with
            | true -> merged
            | false -> Sealed.appendOne (merged.AsSpan()) (RunEdit.Rearrange (start, upTo, at - start, upTo - at, false, false))
        let newClass = match cc = CccAboveBelowFirst with | true -> ModifiedClassBelow | false -> ModifiedClassAbove
        renumber props infos newClass start at upTo moved

    /// <summary>Looks for the modifier combining marks of the classes 220 and 230 and puts them first, <c>reorder_marks_arabic</c>'s
    /// loop over the two classes: it makes the edits of the shifts it finds.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <param name="cc">The class being looked for.</param>
    /// <param name="index">The next mark to look at.</param>
    /// <param name="start">Where the shifted marks go.</param>
    /// <param name="edits">The edits so far.</param>
    /// <returns>The edits.</returns>
    [<TailCall>]
    let rec private shiftClasses (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) finish (cc: int) index start (edits: ImmutableArray<RunEdit>) =
        match cc > CccAboveBelowLast with
        | true -> edits
        | false ->
            let at = skipBelow props finish cc index
            match at = finish with
            | true -> edits
            | false ->
                let upTo = modifierMarksEnd props infos finish cc at
                match at = upTo with
                | true -> shiftClasses props infos finish (cc + 10) at start edits
                | false -> shiftClasses props infos finish (cc + 10) upTo (start + upTo - at) (shiftEdits props infos cc start at upTo edits)

    /// <summary>The edits <c>reorder_marks_arabic</c> makes to a mark run the normalizer has sorted (a modifier combining mark goes
    /// in front of the others of its class, and is renumbered 25 or 26 so that the sequence still sorts).</summary>
    /// <param name="props">The glyph properties, the mark run sorted by modified combining class.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The edits, none when no mark moves.</returns>
    let reorderEdits (props: ReadOnlySpan<GlyphProps>) (infos: ReadOnlySpan<GlyphInfo>) start finish =
        let edits = shiftClasses props infos finish CccAboveBelowFirst start start ImmutableArray.Empty
        match edits.IsEmpty with
        | true -> ImmutableArray<RunEdit>.Empty
        | false -> edits

    /// <summary>Reorders the marks of a mark run the normalizer has sorted, <c>reorder_marks_arabic</c> (a modifier combining mark
    /// goes in front of the others of its class, and is renumbered 25 or 26 so that the sequence still sorts).</summary>
    /// <param name="run">The run, its mark run sorted by modified combining class.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The run with the marks reordered.</returns>
    let reorderMarks (run: GlyphRun) start finish =
        let edits = reorderEdits (run.Props.AsSpan()) (run.Infos.AsSpan()) start finish
        match edits.IsEmpty with
        | true -> run
        | false -> GlyphRun.runPass (EditsPass(edits)) 0 run
