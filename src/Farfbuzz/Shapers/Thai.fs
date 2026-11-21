namespace Farfbuzz

open System

/// <summary>The Thai and Lao shaper's hooks, <c>_hb_ot_shaper_thai</c> (src/hb-ot-shaper-thai.cc:40-388): the SARA AM
/// decomposition and reordering, and the PUA shaping of fonts without a Thai GSUB script.</summary>
[<RequireQualifiedAccess>]
module internal Thai =
    /// <summary>The consonant classes of the Thai PUA shaping, <c>thai_consonant_type_t</c> (src/hb-ot-shaper-thai.cc:40-61).</summary>
    [<Struct>]
    type private ThaiConsonant =
        | NormalConsonant
        | AboveConsonant
        | RemovableConsonant
        | DescenderConsonant
        | NotConsonant

    /// <summary>The mark classes of the Thai PUA shaping, <c>thai_mark_type_t</c> (src/hb-ot-shaper-thai.cc:64-81).</summary>
    [<Struct>]
    type private ThaiMark =
        | AboveVowel
        | BelowVowel
        | ToneMark

    /// <summary>What the Thai PUA shaping does to a mark or to its base, <c>thai_action_t</c> (src/hb-ot-shaper-thai.cc:84-91).</summary>
    [<Struct>]
    type private ThaiAction =
        | NoAction
        | ShiftDown
        | ShiftLeft
        | ShiftDownLeft
        | RemoveDescender

    /// <summary>The state of the marks above a Thai base, <c>thai_above_state_t</c> (src/hb-ot-shaper-thai.cc:152-161).</summary>
    [<Struct>]
    type private ThaiAbove =
        | AboveT0
        | AboveT1
        | AboveT2
        | AboveT3

    /// <summary>The state of the marks below a Thai base, <c>thai_below_state_t</c> (src/hb-ot-shaper-thai.cc:187-193).</summary>
    [<Struct>]
    type private ThaiBelow =
        | BelowB0
        | BelowB1
        | BelowB2

    /// <summary>The bit that distinguishes a Lao code point from its Thai counterpart, <c>0x0080</c>.</summary>
    [<Literal>]
    let private LaoOffset = 0x0080u

    /// <summary>The Thai sara am, <c>0x0E33</c>.</summary>
    [<Literal>]
    let private SaraAm = 0x0E33u

    /// <summary>The Thai nikhahit, <c>0x0E4D</c>.</summary>
    [<Literal>]
    let private Nikhahit = 0x0E4Du

    /// <summary>The first Thai consonant, <c>0x0E01</c>.</summary>
    [<Literal>]
    let private ConsonantFirst = 0x0E01u

    /// <summary>The last Thai consonant, <c>0x0E2E</c>.</summary>
    [<Literal>]
    let private ConsonantLast = 0x0E2Eu

    /// <summary>Whether a code point is a Thai or Lao sara am, <c>IS_SARA_AM</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+0E33 and U+0EB3.</returns>
    let private isSaraAm u =
        (u &&& ~~~LaoOffset) = SaraAm

    /// <summary>Whether a code point is a Thai or Lao mark above the base, <c>IS_ABOVE_BASE_MARK</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for the marks the nikhahit moves over.</returns>
    let private isAboveBaseMark u =
        let thai = u &&& ~~~LaoOffset
        (thai >= 0x0E34u && thai <= 0x0E37u)
        || (thai >= 0x0E47u && thai <= 0x0E4Eu)
        || thai = 0x0E31u
        || thai = 0x0E3Bu

    /// <summary>Classifies a consonant, <c>get_consonant_type</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The consonant class.</returns>
    let private consonantType u =
        match u with
        | 0x0E1Bu | 0x0E1Du | 0x0E1Fu -> AboveConsonant
        | 0x0E0Du | 0x0E10u -> RemovableConsonant
        | 0x0E0Eu | 0x0E0Fu -> DescenderConsonant
        | consonant when consonant >= ConsonantFirst && consonant <= ConsonantLast -> NormalConsonant
        | _ -> NotConsonant

    /// <summary>Classifies a mark, <c>get_mark_type</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The mark class, or none for a code point that is no mark.</returns>
    let private markType u =
        match u with
        | 0x0E31u | 0x0E47u -> ValueSome AboveVowel
        | vowel when (vowel >= 0x0E34u && vowel <= 0x0E37u) || (vowel >= 0x0E4Du && vowel <= 0x0E4Eu) -> ValueSome AboveVowel
        | below when below >= 0x0E38u && below <= 0x0E3Au -> ValueSome BelowVowel
        | tone when tone >= 0x0E48u && tone <= 0x0E4Cu -> ValueSome ToneMark
        | _ -> ValueNone

    /// <summary>The above state a base starts with, <c>thai_above_start_state</c>.</summary>
    /// <param name="consonant">The base's class.</param>
    /// <returns>The state.</returns>
    let private aboveStart (consonant: ThaiConsonant) =
        match consonant with
        | NormalConsonant -> AboveT0
        | AboveConsonant -> AboveT1
        | RemovableConsonant -> AboveT0
        | DescenderConsonant -> AboveT0
        | NotConsonant -> AboveT3

    /// <summary>The below state a base starts with, <c>thai_below_start_state</c>.</summary>
    /// <param name="consonant">The base's class.</param>
    /// <returns>The state.</returns>
    let private belowStart (consonant: ThaiConsonant) =
        match consonant with
        | NormalConsonant -> BelowB0
        | AboveConsonant -> BelowB0
        | RemovableConsonant -> BelowB1
        | DescenderConsonant -> BelowB2
        | NotConsonant -> BelowB2

    /// <summary>The edge of the above state machine, <c>thai_above_state_machine</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="mark">The mark class.</param>
    /// <returns>The action and the next state.</returns>
    let private aboveEdge (state: ThaiAbove) (mark: ThaiMark) =
        match struct (state, mark) with
        | struct (AboveT0, AboveVowel) -> struct (NoAction, AboveT3)
        | struct (AboveT0, BelowVowel) -> struct (NoAction, AboveT0)
        | struct (AboveT0, ToneMark) -> struct (ShiftDown, AboveT3)
        | struct (AboveT1, AboveVowel) -> struct (ShiftLeft, AboveT2)
        | struct (AboveT1, BelowVowel) -> struct (NoAction, AboveT1)
        | struct (AboveT1, ToneMark) -> struct (ShiftDownLeft, AboveT2)
        | struct (AboveT2, AboveVowel) -> struct (NoAction, AboveT3)
        | struct (AboveT2, BelowVowel) -> struct (NoAction, AboveT2)
        | struct (AboveT2, ToneMark) -> struct (ShiftLeft, AboveT3)
        | struct (AboveT3, AboveVowel) -> struct (NoAction, AboveT3)
        | struct (AboveT3, BelowVowel) -> struct (NoAction, AboveT3)
        | struct (AboveT3, ToneMark) -> struct (NoAction, AboveT3)

    /// <summary>The edge of the below state machine, <c>thai_below_state_machine</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="mark">The mark class.</param>
    /// <returns>The action and the next state.</returns>
    let private belowEdge (state: ThaiBelow) (mark: ThaiMark) =
        match struct (state, mark) with
        | struct (BelowB0, AboveVowel) -> struct (NoAction, BelowB0)
        | struct (BelowB0, BelowVowel) -> struct (NoAction, BelowB2)
        | struct (BelowB0, ToneMark) -> struct (NoAction, BelowB0)
        | struct (BelowB1, AboveVowel) -> struct (NoAction, BelowB1)
        | struct (BelowB1, BelowVowel) -> struct (RemoveDescender, BelowB2)
        | struct (BelowB1, ToneMark) -> struct (NoAction, BelowB1)
        | struct (BelowB2, AboveVowel) -> struct (NoAction, BelowB2)
        | struct (BelowB2, BelowVowel) -> struct (ShiftDown, BelowB2)
        | struct (BelowB2, ToneMark) -> struct (NoAction, BelowB2)

    /// <summary>The Windows and Mac PUA forms of a code point for an action, the tables <c>SD_mappings</c>,
    /// <c>SDL_mappings</c>, <c>SL_mappings</c> and <c>RD_mappings</c> of <c>thai_pua_shape</c>.</summary>
    /// <param name="action">The action.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The Windows form and the Mac form, or none.</returns>
    let private puaForms (action: ThaiAction) u =
        match action with
        | NoAction -> ValueNone
        | ShiftDown ->
            match u with
            | 0x0E48u -> ValueSome (struct (0xF70Au, 0xF88Bu))
            | 0x0E49u -> ValueSome (struct (0xF70Bu, 0xF88Eu))
            | 0x0E4Au -> ValueSome (struct (0xF70Cu, 0xF891u))
            | 0x0E4Bu -> ValueSome (struct (0xF70Du, 0xF894u))
            | 0x0E4Cu -> ValueSome (struct (0xF70Eu, 0xF897u))
            | 0x0E38u -> ValueSome (struct (0xF718u, 0xF89Bu))
            | 0x0E39u -> ValueSome (struct (0xF719u, 0xF89Cu))
            | 0x0E3Au -> ValueSome (struct (0xF71Au, 0xF89Du))
            | _ -> ValueNone
        | ShiftDownLeft ->
            match u with
            | 0x0E48u -> ValueSome (struct (0xF705u, 0xF88Cu))
            | 0x0E49u -> ValueSome (struct (0xF706u, 0xF88Fu))
            | 0x0E4Au -> ValueSome (struct (0xF707u, 0xF892u))
            | 0x0E4Bu -> ValueSome (struct (0xF708u, 0xF895u))
            | 0x0E4Cu -> ValueSome (struct (0xF709u, 0xF898u))
            | _ -> ValueNone
        | ShiftLeft ->
            match u with
            | 0x0E48u -> ValueSome (struct (0xF713u, 0xF88Au))
            | 0x0E49u -> ValueSome (struct (0xF714u, 0xF88Du))
            | 0x0E4Au -> ValueSome (struct (0xF715u, 0xF890u))
            | 0x0E4Bu -> ValueSome (struct (0xF716u, 0xF893u))
            | 0x0E4Cu -> ValueSome (struct (0xF717u, 0xF896u))
            | 0x0E31u -> ValueSome (struct (0xF710u, 0xF884u))
            | 0x0E34u -> ValueSome (struct (0xF701u, 0xF885u))
            | 0x0E35u -> ValueSome (struct (0xF702u, 0xF886u))
            | 0x0E36u -> ValueSome (struct (0xF703u, 0xF887u))
            | 0x0E37u -> ValueSome (struct (0xF704u, 0xF888u))
            | 0x0E47u -> ValueSome (struct (0xF712u, 0xF889u))
            | 0x0E4Du -> ValueSome (struct (0xF711u, 0xF899u))
            | _ -> ValueNone
        | RemoveDescender ->
            match u with
            | 0x0E0Du -> ValueSome (struct (0xF70Fu, 0xF89Au))
            | 0x0E10u -> ValueSome (struct (0xF700u, 0xF89Eu))
            | _ -> ValueNone

    /// <summary>Chooses the PUA form the font has, the Windows form before the Mac form, <c>thai_pua_shape</c>'s
    /// <c>hb_font_get_glyph</c> tests.</summary>
    /// <param name="font">The font.</param>
    /// <param name="u">The code point.</param>
    /// <param name="forms">The Windows form and the Mac form.</param>
    /// <returns>The form the font maps, else the code point itself.</returns>
    let private chooseForm (font: Font) u (forms: struct (uint32 * uint32)) =
        let struct (windows, mac) = forms
        match Font.nominalGlyph font windows, Font.nominalGlyph font mac with
        | ValueSome _, _ -> windows
        | ValueNone, ValueSome _ -> mac
        | ValueNone, ValueNone -> u

    /// <summary>Shapes a code point to the PUA form an action asks for, <c>thai_pua_shape</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="action">The action.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The code point to use.</returns>
    let private puaShape (font: Font) (action: ThaiAction) u =
        match puaForms action u with
        | ValueNone -> u
        | ValueSome forms -> chooseForm font u forms

    /// <summary>The state of the SARA AM pass.</summary>
    [<Struct>]
    type private SaraState =
        | SaraOpen
        | SaraScan
        | SaraScratch
        | SaraReplace
        | SaraMerge
        | SaraMove of moveStart: int
        | SaraGraphemes of graphemeStart: int

    /// <summary>Finds where the nikhahit moves to, the <c>while</c> loop over <c>IS_ABOVE_BASE_MARK</c> of
    /// <c>preprocess_text_thai</c>.</summary>
    /// <param name="output">The output built so far.</param>
    /// <param name="start">The candidate start.</param>
    /// <returns>The index of the first of the above-base marks before the nikhahit.</returns>
    [<TailCall>]
    let rec private nikhahitStart (output: ReadOnlySpan<GlyphInfo>) start =
        match start > 0 && isAboveBaseMark output[start - 1].Codepoint with
        | true -> nikhahitStart output (start - 1)
        | false -> start

    /// <summary>The out-buffer pass that decomposes each sara am into a nikhahit and a sara aa and moves the nikhahit back
    /// over the marks above the base, <c>preprocess_text_thai</c>'s first loop.</summary>
    [<Struct>]
    type private SaraPass =
        interface IRunPass<SaraState> with
            member _.Step (cursor, state) =
                match state with
                | SaraOpen -> struct (RunEdit.ClearOutput, SaraScan)
                | SaraScan ->
                    match cursor.Index >= cursor.Input.Length || not cursor.Successful with
                    | true -> struct (RunEdit.Finish, SaraScan)
                    | false ->
                        let info = cursor.Input[cursor.Index]
                        match isSaraAm info.Codepoint with
                        | false -> struct (RunEdit.NextGlyph, SaraScan)
                        | true ->
                            let props = cursor.InputProps[cursor.Index] |> UnicodeProps.setGeneralCategory GeneralCategory.NonSpacingMark |> UnicodeProps.setContinuation
                            struct (RunEdit.OutputInfo ({ info with Codepoint = info.Codepoint - SaraAm + Nikhahit }, props), SaraScratch)
                | SaraScratch -> struct (RunEdit.AddScratch ScratchFlags.HasContinuations, SaraReplace)
                | SaraReplace -> struct (RunEdit.ReplaceGlyph (cursor.Input[cursor.Index].Codepoint - 1u), SaraMerge)
                | SaraMerge ->
                    let finish = cursor.Output.Length
                    let start = nikhahitStart cursor.Output (finish - 2)
                    match start + 2 < finish with
                    | true -> struct (RunEdit.MergeOutClusters (start, finish), SaraMove start)
                    | false -> struct (RunEdit.NextGlyphs 0, SaraGraphemes start)
                | SaraMove moveStart -> struct (RunEdit.MoveOutput (cursor.Output.Length - 2, moveStart), SaraGraphemes moveStart)
                | SaraGraphemes graphemeStart ->
                    match graphemeStart > 0 with
                    | true -> struct (RunEdit.MergeOutGraphemeClusters (graphemeStart - 1, cursor.Output.Length), SaraScan)
                    | false -> struct (RunEdit.NextGlyphs 0, SaraScan)

    /// <summary>Whether any code point of a run is a sara am.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when a sara am is present.</returns>
    [<TailCall>]
    let rec private hasSaraAm (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> false
        | false ->
            match isSaraAm infos[index].Codepoint with
            | true -> true
            | false -> hasSaraAm infos (index + 1)

    /// <summary>Whether any code point of a run is a Thai mark.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when a mark is present.</returns>
    [<TailCall>]
    let rec private hasMark (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> false
        | false ->
            match markType infos[index].Codepoint with
            | ValueSome _ -> true
            | ValueNone -> hasMark infos (index + 1)

    /// <summary>Chooses the action of a mark: at least one of the above and below actions is none, the other is taken,
    /// <c>do_thai_pua_shaping</c>'s <c>action</c>.</summary>
    /// <param name="aboveAction">The action of the above state machine.</param>
    /// <param name="belowAction">The action of the below state machine.</param>
    /// <returns>The action to apply.</returns>
    let private chosenAction aboveAction belowAction =
        match aboveAction with
        | NoAction -> belowAction
        | ShiftDown | ShiftLeft | ShiftDownLeft | RemoveDescender -> aboveAction

    /// <summary>The state of the PUA pass.</summary>
    [<Struct>]
    type private PuaState =
        | PuaScan of scanAbove: ThaiAbove * scanBelow: ThaiBelow * scanBase: int
        | PuaShape of shapeAbove: ThaiAbove * shapeBelow: ThaiBelow * shapeBase: int * shapeAction: ThaiAction
        | PuaAdvance of advanceAbove: ThaiAbove * advanceBelow: ThaiBelow * advanceBase: int

    /// <summary>The in-place pass that shifts marks and removes descenders through PUA code points, <c>do_thai_pua_shaping</c>.</summary>
    [<Struct>]
    type private PuaPass =
        val Font: Font

        /// <summary>Creates the pass.</summary>
        /// <param name="font">The font whose cmap decides which PUA form exists.</param>
        new (font) = { Font = font }

        interface IRunPass<PuaState> with
            member this.Step (cursor, state) =
                match state with
                | PuaScan (above, below, basis) ->
                    match cursor.Index >= cursor.Input.Length with
                    | true -> struct (RunEdit.Finish, state)
                    | false ->
                        let codepoint = cursor.Input[cursor.Index].Codepoint
                        match markType codepoint with
                        | ValueNone ->
                            let consonant = consonantType codepoint
                            struct (RunEdit.NextGlyph, PuaScan (aboveStart consonant, belowStart consonant, cursor.Index))
                        | ValueSome mark ->
                            let struct (aboveAction, nextAbove) = aboveEdge above mark
                            let struct (belowAction, nextBelow) = belowEdge below mark
                            struct (RunEdit.UnsafeToBreak (basis, cursor.Index), PuaShape (nextAbove, nextBelow, basis, chosenAction aboveAction belowAction))
                | PuaShape (above, below, basis, action) ->
                    let target =
                        match action with
                        | RemoveDescender -> basis
                        | NoAction | ShiftDown | ShiftLeft | ShiftDownLeft -> cursor.Index
                    let before = cursor.Input[target].Codepoint
                    let shaped = puaShape this.Font action before
                    match shaped = before with
                    | true -> struct (RunEdit.NextGlyphs 0, PuaAdvance (above, below, basis))
                    | false -> struct (RunEdit.SetInputGlyph (target, shaped, cursor.InputProps[target]), PuaAdvance (above, below, basis))
                | PuaAdvance (above, below, basis) -> struct (RunEdit.NextGlyph, PuaScan (above, below, basis))

    /// <summary>Applies the Thai PUA shaping, <c>do_thai_pua_shaping</c>, to a run with a mark.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let private puaShaping (font: Font) (run: GlyphRun) =
        match hasMark (run.Infos.AsSpan()) 0 with
        | false -> run
        | true -> GlyphRun.runPass (PuaPass(font)) (PuaScan (aboveStart NotConsonant, belowStart NotConsonant, 0)) run

    /// <summary>Prepares a run for the Thai and Lao shaper, <c>preprocess_text_thai</c>: every sara am is decomposed and its
    /// nikhahit moved over the marks above the base; a font with no GSUB script for Thai gets the PUA shaping too.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The prepared run.</returns>
    let preprocess (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let decomposed =
            match hasSaraAm (run.Infos.AsSpan()) 0 with
            | false -> run
            | true -> GlyphRun.runPass (SaraPass()) SaraOpen run
        match plan.Props.Script = Scripts.Thai && not plan.Map.FoundGsubScript with
        | true -> puaShaping font decomposed
        | false -> decomposed

    /// <summary>Prepares a working set for the Thai and Lao shaper as <see cref="preprocess"/> does: a set with no sara am and a font with a
    /// GSUB script for Thai is left as it is, and the others take the pass on a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set.</param>
    /// <returns>The prepared set.</returns>
    let preprocessSet (plan: OtShapePlan) (font: Font) (set: WorkingSet) =
        match hasSaraAm (set.Infos.AsSpan()) 0 || (plan.Props.Script = Scripts.Thai && not plan.Map.FoundGsubScript) with
        | false -> set
        | true -> GlyphRun.setOf (preprocess plan font (GlyphRun.runOf set))
