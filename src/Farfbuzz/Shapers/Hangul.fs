namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Hangul shaper's hooks, <c>_hb_ot_shaper_hangul</c> (src/hb-ot-shaper-hangul.cc:37-449): the jamo features,
/// the composition and decomposition of syllables by what the font has, the tone marks, and the feature masks.</summary>
[<RequireQualifiedAccess>]
module internal Hangul =
    /// <summary>The feature <c>ljmo</c>, the leading consonants.</summary>
    [<Literal>]
    let private Ljmo = 0x6C6A6D6Fu<tag>

    /// <summary>The feature <c>vjmo</c>, the vowels.</summary>
    [<Literal>]
    let private Vjmo = 0x766A6D6Fu<tag>

    /// <summary>The feature <c>tjmo</c>, the trailing consonants.</summary>
    [<Literal>]
    let private Tjmo = 0x746A6D6Fu<tag>

    /// <summary>The value of <c>hangul_shaping_feature</c> of a glyph that takes the <c>ljmo</c> mask.</summary>
    [<Literal>]
    let private LjmoFeature = 1uy

    /// <summary>The value of <c>hangul_shaping_feature</c> of a glyph that takes the <c>vjmo</c> mask.</summary>
    [<Literal>]
    let private VjmoFeature = 2uy

    /// <summary>The value of <c>hangul_shaping_feature</c> of a glyph that takes the <c>tjmo</c> mask.</summary>
    [<Literal>]
    let private TjmoFeature = 3uy

    /// <summary>The first leading consonant, <c>LBase</c>.</summary>
    [<Literal>]
    let private LBase = 0x1100u

    /// <summary>The first vowel, <c>VBase</c>.</summary>
    [<Literal>]
    let private VBase = 0x1161u

    /// <summary>The code point before the first trailing consonant, <c>TBase</c>.</summary>
    [<Literal>]
    let private TBase = 0x11A7u

    /// <summary>The first precomposed syllable, <c>SBase</c>.</summary>
    [<Literal>]
    let private SBase = 0xAC00u

    /// <summary>The number of leading consonants that compose, <c>LCount</c>.</summary>
    [<Literal>]
    let private LCount = 19u

    /// <summary>The number of vowels that compose, <c>VCount</c>.</summary>
    [<Literal>]
    let private VCount = 21u

    /// <summary>The number of trailing consonants that compose, plus the empty one, <c>TCount</c>.</summary>
    [<Literal>]
    let private TCount = 28u

    /// <summary>The number of syllables of one leading consonant, <c>NCount</c>.</summary>
    [<Literal>]
    let private NCount = VCount * TCount

    /// <summary>The number of precomposed syllables, <c>SCount</c>.</summary>
    [<Literal>]
    let private SCount = LCount * NCount

    /// <summary>The dotted circle, <c>0x25CC</c>.</summary>
    [<Literal>]
    let private DottedCircle = 0x25CCu

    /// <summary>Whether a code point is a leading consonant that composes, <c>isCombiningL</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1100 to U+1112.</returns>
    let private isCombiningL u =
        u >= LBase && u <= LBase + LCount - 1u

    /// <summary>Whether a code point is a vowel that composes, <c>isCombiningV</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1161 to U+1175.</returns>
    let private isCombiningV u =
        u >= VBase && u <= VBase + VCount - 1u

    /// <summary>Whether a code point is a trailing consonant that composes, <c>isCombiningT</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+11A8 to U+11C2.</returns>
    let private isCombiningT u =
        u >= TBase + 1u && u <= TBase + TCount - 1u

    /// <summary>Whether a code point is a precomposed syllable, <c>isCombinedS</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+AC00 to U+D7A3.</returns>
    let private isCombinedS u =
        u >= SBase && u <= SBase + SCount - 1u

    /// <summary>Whether a code point is a leading consonant, <c>isL</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1100 to U+115F and U+A960 to U+A97C.</returns>
    let private isL u =
        (u >= 0x1100u && u <= 0x115Fu) || (u >= 0xA960u && u <= 0xA97Cu)

    /// <summary>Whether a code point is a vowel, <c>isV</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1160 to U+11A7 and U+D7B0 to U+D7C6.</returns>
    let private isV u =
        (u >= 0x1160u && u <= 0x11A7u) || (u >= 0xD7B0u && u <= 0xD7C6u)

    /// <summary>Whether a code point is a trailing consonant, <c>isT</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+11A8 to U+11FF and U+D7CB to U+D7FB.</returns>
    let private isT u =
        (u >= 0x11A8u && u <= 0x11FFu) || (u >= 0xD7CBu && u <= 0xD7FBu)

    /// <summary>Whether a code point is a Hangul tone mark, <c>isHangulTone</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+302E and U+302F.</returns>
    let private isHangulTone u =
        u >= 0x302Eu && u <= 0x302Fu

    /// <summary>Adds the jamo features, <c>collect_features_hangul</c>.</summary>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with <c>ljmo</c>, <c>vjmo</c> and <c>tjmo</c> added.</returns>
    let collectFeatures builder =
        builder
        |> MapBuilder.addFeature Ljmo FeatureFlags.None 1u
        |> MapBuilder.addFeature Vjmo FeatureFlags.None 1u
        |> MapBuilder.addFeature Tjmo FeatureFlags.None 1u

    /// <summary>Adds <c>calt</c> once more, to allocate a mask of its own, <c>override_features_hangul</c>: the mask is
    /// cleared on the jamo only, which keeps them out of the lookups of fonts that apply all of their jamo shaping in
    /// <c>calt</c>.</summary>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with <c>calt</c> added.</returns>
    let overrideFeatures builder =
        MapBuilder.addFeature Tags.Calt FeatureFlags.None 1u builder

    /// <summary>Reads the masks of the jamo features from the compiled map, <c>data_create_hangul</c>.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <returns>The plan data.</returns>
    let createData mask1 =
        HangulData
            { Ljmo = mask1 Ljmo
              Vjmo = mask1 Vjmo
              Tjmo = mask1 Tjmo
              Calt = mask1 Tags.Calt }

    /// <summary>The mask of the jamo feature a glyph was given, <c>mask_array[hangul_shaping_feature ()]</c>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="feature">The glyph's jamo feature, zero for none.</param>
    /// <returns>The mask, zero for a glyph without a feature.</returns>
    let private featureMask (data: HangulPlan) feature =
        match feature with
        | LjmoFeature -> data.Ljmo
        | VjmoFeature -> data.Vjmo
        | TjmoFeature -> data.Tjmo
        | _ -> 0u

    /// <summary>Sets the masks of the jamo features and keeps <c>calt</c> away from the jamo, <c>setup_masks_hangul</c>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, normalised and with its global masks set.</param>
    /// <returns>The run with the masks set.</returns>
    let setupMasks (data: HangulPlan) (run: GlyphRun) =
        let infos =
            Sealed.mapi (fun index (info: GlyphInfo) ->
                let mask = info.Mask ||| featureMask data run.Props[index].ShaperAuxiliary
                match isL info.Codepoint || isV info.Codepoint || isT info.Codepoint with
                | true -> { info with Mask = mask &&& ~~~data.Calt }
                | false -> { info with Mask = mask }) (run.Infos.AsSpan())
        { run with Infos = infos }

    /// <summary>Whether a font gives a code point a glyph, <c>font->has_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="u">The code point.</param>
    /// <returns>True when the font maps it.</returns>
    let private hasGlyph (font: Font) u =
        match Font.nominalGlyph font u with
        | ValueSome _ -> true
        | ValueNone -> false

    /// <summary>Whether a font gives a code point a glyph with no advance, <c>is_zero_width_char</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="u">The code point.</param>
    /// <returns>True when the glyph is zero width.</returns>
    let private isZeroWidthChar (font: Font) u =
        match Font.nominalGlyph font u with
        | ValueSome glyph -> Font.hAdvance font glyph = 0
        | ValueNone -> false

    /// <summary>What the pass does once the edits of one step of the loop are made.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private HangulAfter =
        | AfterScan of scanStart: int * scanFinish: int
        | AfterJamo of jamoStart: int * jamoFinish: int

    /// <summary>The state of the preprocess pass: the loop of <c>preprocess_text_hangul</c>, with the extent of the
    /// syllable last seen (<c>start</c> and <c>end</c>, as indices of the output), and the edits of one step.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private HangulState =
        | HangulOpen
        | HangulScan of start: int * finish: int
        | HangulEmit of edits: ImmutableArray<RunEdit> * index: int * after: HangulAfter
        | HangulJamo of jamoFirst: int * jamoEnd: int
        | HangulFeature of featureFirst: int * featureEnd: int * featureNext: int
        | HangulSkip of skipCount: int * skipAfter: HangulAfter
        | HangulCompose of composeCount: int * composed: uint32 * composeAfter: HangulAfter

    /// <summary>The state that follows the last edit of a step.</summary>
    /// <param name="after">What follows.</param>
    /// <returns>The state.</returns>
    let private resume after =
        match after with
        | AfterScan (start, finish) -> HangulScan (start, finish)
        | AfterJamo (first, finish) -> HangulJamo (first, finish)

    /// <summary>The edit at an index of a step's edits, with the state that follows it.</summary>
    /// <param name="edits">The edits of the step.</param>
    /// <param name="index">The edit to make.</param>
    /// <param name="after">What follows the last edit.</param>
    /// <returns>The edit and the next state.</returns>
    let private emitFrom (edits: ImmutableArray<RunEdit>) index after =
        match index + 1 < edits.Length with
        | true -> struct (edits[index], HangulEmit (edits, index + 1, after))
        | false -> struct (edits[index], resume after)

    /// <summary>The one edit of a step, with the state that follows it.</summary>
    /// <param name="edit">The edit.</param>
    /// <param name="after">What follows it.</param>
    /// <returns>The edit and the next state.</returns>
    let private single edit after =
        struct (edit, resume after)

    /// <summary>The edits for a tone mark: moved in front of the syllable it follows unless it is zero width, or given a
    /// dotted circle to stand on, or left alone.</summary>
    /// <param name="font">The font.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="u">The tone mark.</param>
    /// <param name="start">The start of the last syllable, in the output.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The first edit and the state that follows it.</returns>
    let private toneEdits (font: Font) flags (cursor: RunCursor) u start finish =
        let outLen = cursor.Output.Length
        let zeroWidth = isZeroWidthChar font u
        match start < finish && finish = outLen with
        | true ->
            let after = AfterScan (outLen + 1, outLen + 1)
            match zeroWidth with
            | true -> emitFrom (ImmutableArray.Create (RunEdit.UnsafeToBreakFromOutbuffer (start, cursor.Index), RunEdit.NextGlyph)) 0 after
            | false ->
                emitFrom (Sealed.ofSpan (ReadOnlySpan [|
                    RunEdit.UnsafeToBreakFromOutbuffer (start, cursor.Index)
                    RunEdit.NextGlyph
                    RunEdit.MergeOutClusters (start, finish + 1)
                    RunEdit.MoveOutput (finish, start)
                |])) 0 after
        | false ->
            match flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default && hasGlyph font DottedCircle with
            | false -> single RunEdit.NextGlyph (AfterScan (outLen + 1, outLen + 1))
            | true ->
                let chars =
                    match zeroWidth with
                    | true -> Sealed.ofSpan (ReadOnlySpan [| DottedCircle; u |])
                    | false -> Sealed.ofSpan (ReadOnlySpan [| u; DottedCircle |])
                single (RunEdit.ReplaceGlyphs (1, chars)) (AfterScan (outLen + 2, outLen + 2))

    /// <summary>The edits for a leading consonant followed by a vowel and maybe a trailing consonant: composed into one
    /// syllable when the font has it, else each glyph is given its jamo feature.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="l">The leading consonant.</param>
    /// <param name="v">The vowel.</param>
    /// <returns>The first edit and the state that follows it.</returns>
    let private jamoEdits (font: Font) (cursor: RunCursor) l v =
        let outLen = cursor.Output.Length
        let index = cursor.Index
        let afterV = index + 2
        let t =
            match afterV < cursor.Input.Length && isT cursor.Input[afterV].Codepoint with
            | true -> cursor.Input[afterV].Codepoint
            | false -> 0u
        let tindex = match t with | 0u -> 0u | _ -> t - TBase
        let count = match t with | 0u -> 2 | _ -> 3
        let flag = RunEdit.UnsafeToBreak (index, index + count)
        let s = SBase + (l - LBase) * NCount + (v - VBase) * TCount + tindex
        match isCombiningL l && isCombiningV v && (t = 0u || isCombiningT t) && hasGlyph font s with
        | true -> struct (flag, HangulCompose (count, s, AfterScan (outLen, outLen + 1)))
        | false -> struct (flag, HangulSkip (count, AfterJamo (outLen, outLen + count)))

    /// <summary>The edits for a precomposed syllable in the text: kept when the font has it, combined with a trailing
    /// consonant that follows when the font has the result, else decomposed into jamo.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="s">The syllable.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The first edit and the state that follows it.</returns>
    let private syllableEdits (font: Font) (cursor: RunCursor) s finish =
        let outLen = cursor.Output.Length
        let index = cursor.Index
        let hasS = hasGlyph font s
        let lindex = (s - SBase) / NCount
        let vindex = ((s - SBase) % NCount) / TCount
        let tindex = (s - SBase) % TCount
        let following = match index + 1 < cursor.Input.Length with | true -> cursor.Input[index + 1].Codepoint | false -> 0u
        let combines = tindex = 0u && isCombiningT following
        let newS = s + (following - TBase)
        match combines && hasGlyph font newS with
        | true -> struct (RunEdit.ReplaceGlyphs (2, Sealed.ofSpan (ReadOnlySpan [| newS |])), HangulScan (outLen, outLen + 1))
        | false ->
            let trailing = tindex = 0u && isT following
            let leading = LBase + lindex
            let vowel = VBase + vindex
            let tail = TBase + tindex
            let canDecompose = (not hasS || trailing) && hasGlyph font leading && hasGlyph font vowel && (tindex = 0u || hasGlyph font tail)
            let flag = RunEdit.UnsafeToBreak (index, index + 2)
            match canDecompose with
            | true ->
                let length = match tindex with | 0u -> 2 | _ -> 3
                let jamo = match tindex with | 0u -> Sealed.ofSpan (ReadOnlySpan [| leading; vowel |]) | _ -> Sealed.ofSpan (ReadOnlySpan [| leading; vowel; tail |])
                let replace = RunEdit.ReplaceGlyphs (1, jamo)
                let after = AfterJamo (outLen, outLen + length + (match hasS && tindex = 0u with | true -> 1 | false -> 0))
                match combines, hasS && tindex = 0u with
                | true, true -> emitFrom (ImmutableArray.Create (flag, replace, RunEdit.NextGlyph)) 0 after
                | true, false -> emitFrom (ImmutableArray.Create (flag, replace)) 0 after
                | false, true -> emitFrom (ImmutableArray.Create (replace, RunEdit.NextGlyph)) 0 after
                | false, false -> single replace after
            | false ->
                let after = AfterScan (outLen, (match hasS with | true -> outLen + 1 | false -> finish))
                match combines || trailing with
                | true -> struct (flag, HangulSkip (1, after))
                | false -> single RunEdit.NextGlyph after

    /// <summary>The edits for a glyph that is no tone mark: a syllable, the start of one, or none.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="u">The code point.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The first edit and the state that follows it.</returns>
    let private plainEdits (font: Font) (cursor: RunCursor) u finish =
        let index = cursor.Index
        let none = single RunEdit.NextGlyph (AfterScan (cursor.Output.Length, finish))
        match isL u && index + 1 < cursor.Input.Length with
        | true ->
            match isV cursor.Input[index + 1].Codepoint with
            | true -> jamoEdits font cursor u cursor.Input[index + 1].Codepoint
            | false -> none
        | false ->
            match isCombinedS u with
            | true -> syllableEdits font cursor u finish
            | false -> none

    /// <summary>The edit that gives one output glyph its jamo feature.</summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="index">The glyph, in the output.</param>
    /// <param name="feature">The feature.</param>
    /// <returns>The edit.</returns>
    let private setFeature (cursor: RunCursor) index feature =
        RunEdit.SetOutputGlyph (index, cursor.Output[index].Codepoint, { cursor.OutputProps[index] with ShaperAuxiliary = feature })

    /// <summary>The edit that follows another in the features of a decomposed syllable: the next glyph gets its jamo feature, the
    /// last trailing consonant only when there are three, then the clusters merge.</summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="first">The first glyph of the syllable, in the output.</param>
    /// <param name="finish">The end of the syllable, in the output.</param>
    /// <param name="next">The glyph to give its feature, or the end of the syllable when the clusters merge.</param>
    /// <returns>The edit and the state that follows it.</returns>
    let private featureEdit (cursor: RunCursor) first finish next =
        match next - first with
        | 0 -> struct (setFeature cursor next LjmoFeature, HangulFeature (first, finish, next + 1))
        | 1 -> struct (setFeature cursor next VjmoFeature, HangulFeature (first, finish, next + 1))
        | 2 when finish - first = 3 -> struct (setFeature cursor next TjmoFeature, HangulFeature (first, finish, next + 1))
        | _ -> single (RunEdit.MergeOutGraphemeClusters (first, finish)) (AfterScan (first, finish))

    /// <summary>The pass of <c>preprocess_text_hangul</c>.</summary>
    [<Struct>]
    type private HangulPass =
        val Font: Font

        /// <summary>Creates the pass.</summary>
        /// <param name="font">The font, whose cmap decides what composes.</param>
        new (font) = { Font = font }

        interface IRunPass<HangulState> with
            member this.Step (cursor, state) =
                match state with
                | HangulOpen -> struct (RunEdit.ClearOutput, HangulScan (0, 0))
                | HangulEmit (edits, index, after) -> emitFrom edits index after
                | HangulJamo (first, finish) ->
                    match cursor.Successful with
                    | true -> featureEdit cursor first finish first
                    | false -> struct (RunEdit.Finish, HangulScan (first, finish))
                | HangulFeature (first, finish, next) ->
                    match cursor.Successful with
                    | true -> featureEdit cursor first finish next
                    | false -> struct (RunEdit.Finish, HangulScan (first, finish))
                | HangulSkip (count, after) -> struct (RunEdit.NextGlyphs count, resume after)
                | HangulCompose (count, composed, after) -> struct (RunEdit.ReplaceGlyphs (count, Sealed.ofSpan (ReadOnlySpan [| composed |])), resume after)
                | HangulScan (start, finish) ->
                    match cursor.Index >= cursor.Input.Length || not cursor.Successful with
                    | true -> struct (RunEdit.Finish, HangulScan (start, finish))
                    | false ->
                        let u = cursor.Input[cursor.Index].Codepoint
                        match isHangulTone u with
                        | true -> toneEdits this.Font cursor.Flags cursor u start finish
                        | false -> plainEdits this.Font cursor u finish

    /// <summary>What the owned preprocess asks of a font: the cmap and the advances, as <see cref="hasGlyph"/> and
    /// <see cref="isZeroWidthChar"/> answer them.</summary>
    [<Struct>]
    type private FontGlyphs =
        val Font: Font

        /// <summary>Creates the view.</summary>
        /// <param name="font">The font.</param>
        new (font) = { Font = font }

        interface IHangulGlyphs with
            member this.Has u = hasGlyph this.Font u
            member this.ZeroWidth u = isZeroWidthChar this.Font u

    /// <summary>What the owned preprocess asks of a font, with the presence of the Hangul code points read from the bits of the face's
    /// presence table instead of its cmap: the same answers as <see cref="FontGlyphs"/>, a page of the cmap built once for 256 code points.</summary>
    [<Struct>]
    type private PresentGlyphs =
        val Font: Font
        val Accel: CmapAccel
        val Presence: PresencePages

        /// <summary>Creates the view.</summary>
        /// <param name="font">The font.</param>
        /// <param name="accel">The font's cmap accelerator.</param>
        /// <param name="presence">The face's presence table.</param>
        new (font, accel, presence) = { Font = font; Accel = accel; Presence = presence }

        interface IHangulGlyphs with
            member this.Has u =
                match this.Font.Funcs with
                | ValueSome _ -> ValueOption.isSome (Font.nominalGlyph this.Font u)
                | ValueNone -> Cmap.hasGlyph this.Accel this.Presence u
            member this.ZeroWidth u = isZeroWidthChar this.Font u

    /// <summary>The view of a font the owned routes use, which fetches the face's presence table once.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The view.</returns>
    let private presentGlyphs (font: Font) =
        let accel = font.Ot.Cmap.Value
        PresentGlyphs(font, accel, Cmap.presenceOf accel)

    /// <summary>Whether the glyph at an index can make the preprocess pass change the run: a tone mark, a leading consonant before a
    /// vowel, or a syllable that the font lacks or that a trailing consonant follows; every other glyph is only advanced.</summary>
    /// <param name="glyphs">The font's view of what composes.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>True when the pass may act on it.</returns>
    let private startsChange<'F when 'F : struct and 'F :> IHangulGlyphs> (glyphs: 'F) (infos: ReadOnlySpan<GlyphInfo>) index =
        let u = infos[index].Codepoint
        let following = match index + 1 < infos.Length with | true -> infos[index + 1].Codepoint | false -> 0u
        match isHangulTone u, isL u, isCombinedS u with
        | true, _, _ -> true
        | false, true, _ -> isV following
        | false, false, true -> not (glyphs.Has u) || ((u - SBase) % TCount = 0u && isT following)
        | false, false, false -> false

    /// <summary>Whether any glyph of a run can make the preprocess pass change it.</summary>
    /// <param name="glyphs">The font's view of what composes.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when the pass may act on the run.</returns>
    [<TailCall>]
    let rec private hasChange<'F when 'F : struct and 'F :> IHangulGlyphs> (glyphs: 'F) (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> false
        | false ->
            match startsChange glyphs infos index with
            | true -> true
            | false -> hasChange glyphs infos (index + 1)

    /// <summary>Composes the syllables of a run when the font has them, decomposes those it lacks into the jamo, gives each
    /// jamo its feature and moves the tone marks in front of their syllable, <c>preprocess_text_hangul</c>: a run that the pass would only
    /// advance through is returned as it is.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The prepared run.</returns>
    let preprocess (font: Font) (run: GlyphRun) =
        match hasChange (FontGlyphs(font)) (run.Infos.AsSpan()) 0 with
        | false -> run
        | true -> GlyphRun.runPass (HangulPass(font)) HangulOpen run

    /// <summary>The preprocess of <see cref="preprocess"/> on the out-buffer machine without the pass interpreter: the same edits in the same
    /// order, issued by one loop over the machine, so that a run that changes is rewritten at the cost of its edits and not of a step
    /// each, in the arrays of the run when it has no positions: the run must be one whose arrays the caller made and drops. The run it
    /// answers is that of <see cref="preprocess"/> in every field.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The prepared run.</returns>
    let preprocessOwned (font: Font) (run: GlyphRun) =
        let glyphs = presentGlyphs font
        match hasChange glyphs (run.Infos.AsSpan()) 0 with
        | false -> run
        | true ->
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
            let result =
                match run.HavePositions || run.Props.Length <> run.Infos.Length with
                | true -> Sealed.runHangul input glyphs
                | false -> Sealed.runHangulOwned input glyphs
            match result.Unchanged with
            | true -> run
            | false ->
                { run with
                    Infos = result.Infos
                    Props = result.Props
                    Positions = result.Positions
                    HavePositions = result.HavePositions
                    MaxOps = result.MaxOps
                    Successful = result.Successful
                    Serial = uint32 result.Serial
                    RandomState = result.RandomState
                    ScratchFlags = result.ScratchFlags }

    /// <summary>Sets the masks of the jamo features as <see cref="setupMasks"/> does, in the glyph array of a run whose arrays the caller
    /// made in this call and shared with no one.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, normalised and with its global masks set, with owned arrays.</param>
    /// <returns>The run with the masks set.</returns>
    let setupMasksOwned (data: HangulPlan) (run: GlyphRun) =
        Sealed.hangulMasksOwned data.Ljmo data.Vjmo data.Tjmo data.Calt run.Infos run.Props
        run


    /// <summary>The preprocess of <see cref="preprocessOwned"/> on a working set whose arrays the caller made in this call and shares with no
    /// one: the set it answers is that of <see cref="preprocess"/> in every field.</summary>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set.</param>
    /// <returns>The prepared set.</returns>
    let preprocessSet (font: Font) (set: WorkingSet) =
        let glyphs = presentGlyphs font
        match hasChange glyphs (set.Infos.AsSpan()) 0 with
        | false -> set
        | true ->
            let input = GlyphRun.inputOfSet set
            let result =
                match set.HavePositions || set.Props.Length <> set.Infos.Length with
                | true -> Sealed.runHangul input glyphs
                | false -> Sealed.runHangulOwned input glyphs
            GlyphRun.applyResult set result

    /// <summary>Sets the masks of the jamo features as <see cref="setupMasksOwned"/> does, in the glyph array of a working set.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="set">The working set, normalised and with its global masks set, with owned arrays.</param>
    /// <returns>The set with the masks set.</returns>
    let setupMasksSet (data: HangulPlan) (set: WorkingSet) =
        Sealed.hangulMasksOwned data.Ljmo data.Vjmo data.Tjmo data.Calt set.Infos set.Props
        set
