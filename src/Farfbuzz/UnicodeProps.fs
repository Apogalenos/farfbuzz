namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Unicode property packing of <c>hb-ot-layout.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal UnicodeProps =
    /// <summary>Returns whether a general category is a mark.</summary>
    /// <param name="category">The category.</param>
    /// <returns>True for spacing, enclosing and non-spacing marks.</returns>
    let isMark (category: GeneralCategory) =
        match category with
        | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
        | GeneralCategory.Control | GeneralCategory.Format | GeneralCategory.Unassigned | GeneralCategory.PrivateUse
        | GeneralCategory.Surrogate | GeneralCategory.LowercaseLetter | GeneralCategory.ModifierLetter
        | GeneralCategory.OtherLetter | GeneralCategory.TitlecaseLetter | GeneralCategory.UppercaseLetter
        | GeneralCategory.DecimalNumber | GeneralCategory.LetterNumber | GeneralCategory.OtherNumber
        | GeneralCategory.ConnectPunctuation | GeneralCategory.DashPunctuation | GeneralCategory.ClosePunctuation
        | GeneralCategory.FinalPunctuation | GeneralCategory.InitialPunctuation | GeneralCategory.OtherPunctuation
        | GeneralCategory.OpenPunctuation | GeneralCategory.CurrencySymbol | GeneralCategory.ModifierSymbol
        | GeneralCategory.MathSymbol | GeneralCategory.OtherSymbol | GeneralCategory.LineSeparator
        | GeneralCategory.ParagraphSeparator | GeneralCategory.SpaceSeparator -> false

    /// <summary>The general category's numeric property value.</summary>
    /// <param name="category">The category.</param>
    /// <returns>The value from 0 to 29.</returns>
    let private categoryValue (category: GeneralCategory) =
        match category with
        | GeneralCategory.Control -> 0 | GeneralCategory.Format -> 1 | GeneralCategory.Unassigned -> 2
        | GeneralCategory.PrivateUse -> 3 | GeneralCategory.Surrogate -> 4 | GeneralCategory.LowercaseLetter -> 5
        | GeneralCategory.ModifierLetter -> 6 | GeneralCategory.OtherLetter -> 7 | GeneralCategory.TitlecaseLetter -> 8
        | GeneralCategory.UppercaseLetter -> 9 | GeneralCategory.SpacingMark -> 10 | GeneralCategory.EnclosingMark -> 11
        | GeneralCategory.NonSpacingMark -> 12 | GeneralCategory.DecimalNumber -> 13 | GeneralCategory.LetterNumber -> 14
        | GeneralCategory.OtherNumber -> 15 | GeneralCategory.ConnectPunctuation -> 16 | GeneralCategory.DashPunctuation -> 17
        | GeneralCategory.ClosePunctuation -> 18 | GeneralCategory.FinalPunctuation -> 19 | GeneralCategory.InitialPunctuation -> 20
        | GeneralCategory.OtherPunctuation -> 21 | GeneralCategory.OpenPunctuation -> 22 | GeneralCategory.CurrencySymbol -> 23
        | GeneralCategory.ModifierSymbol -> 24 | GeneralCategory.MathSymbol -> 25 | GeneralCategory.OtherSymbol -> 26
        | GeneralCategory.LineSeparator -> 27 | GeneralCategory.ParagraphSeparator -> 28 | GeneralCategory.SpaceSeparator -> 29

    /// <summary>The hidden/default-ignorable bit and its scratch flag.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The property bits and scratch flags.</returns>
    let private ignorableBits (u) =
        match Unicode.isDefaultIgnorable u with
        | false -> struct (0, ScratchFlags.Default)
        | true ->
            let flags = ScratchFlags.HasDefaultIgnorables
            let bits =
                match u with
                | 0x200Cu -> 0x0200
                | 0x200Du -> 0x0100
                | 0x180Bu | 0x180Cu | 0x180Du | 0x180Fu -> 0x0040
                | u when u >= 0xE0020u && u <= 0xE007Fu -> 0x0040
                | 0x034Fu -> 0x0040
                | _ -> 0
            let flags = match u with | 0x034Fu -> flags ||| ScratchFlags.HasCgj | _ -> flags
            struct (0x0020 ||| bits, flags)

    /// <summary>Assembles the packed unicode properties and scratch bits from the answers of the functions.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="category">The general category.</param>
    /// <param name="modifiedClass">The modified combining class, asked for a mark only.</param>
    /// <returns>The packed properties and scratch flags.</returns>
    let inline private assemble (u: uint32) category ([<InlineIfLambda>] modifiedClass: unit -> byte) =
        let baseProps = categoryValue category
        match u >= 0x80u with
        | false -> struct (uint16 baseProps, ScratchFlags.Default)
        | true ->
            let struct (ignorable, scratch) = ignorableBits u
            let props = baseProps ||| ignorable
            match isMark category with
            | false -> struct (uint16 props, scratch)
            | true ->
                let continuation = ScratchFlags.HasContinuations
                let props = props ||| 0x0080 ||| (int (modifiedClass ()) <<< 8)
                struct (uint16 props, scratch ||| continuation)

    /// <summary>Computes the packed unicode properties and scratch bits, <c>_hb_glyph_info_set_unicode_props</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The packed properties and scratch flags.</returns>
    let compute (u) =
        assemble u (Unicode.generalCategory u) (fun () -> Unicode.modifiedCombiningClass u)

    /// <summary>Computes the packed unicode properties and scratch bits through replaced Unicode functions.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The packed properties and scratch flags.</returns>
    let computeWith (funcs: UnicodeFuncs) (u) =
        assemble u (funcs.GeneralCategory u) (fun () -> Unicode.modifiedBy funcs u)

    /// <summary>The packed Unicode properties of U+0000 to U+00FF, precomputed once from <see cref="compute"/>.</summary>
    let latinTable : ImmutableArray<struct (uint16 * ScratchFlags)> =
        Sealed.init 256 (fun u -> compute (uint32 u))

    /// <summary>Computes the packed Unicode properties, reading the precomputed table for U+0000 to U+00FF.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The packed properties and scratch flags.</returns>
    let computeFast (u: uint32) : struct (uint16 * ScratchFlags) =
        match u < 256u with
        | true -> latinTable[int u]
        | false -> compute u

    /// <summary>The number of code points whose packed properties <see cref="lowTable"/> holds: U+0000 to U+20FF, the Latin, Greek,
    /// Cyrillic, Armenian, Hebrew, Arabic, Syriac, Indic, Thai and Lao blocks and the general punctuation, superscripts and currency
    /// symbols.</summary>
    [<Literal>]
    let LowLimit = 0x2100

    /// <summary>The packed Unicode properties of U+0000 to U+20FF, precomputed once from <see cref="compute"/>.</summary>
    let lowTable : ImmutableArray<struct (uint16 * ScratchFlags)> =
        Sealed.init LowLimit (fun u -> compute (uint32 u))

    /// <summary>Computes the packed Unicode properties, reading the precomputed table for U+0000 to U+20FF.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The packed properties and scratch flags.</returns>
    let computeLow (u: uint32) =
        match u < uint32 LowLimit with
        | true -> lowTable[int u]
        | false -> compute u

    /// <summary>The general-category field mask, <c>UPROPS_MASK_GEN_CAT</c>.</summary>
    [<Literal>]
    let GenCatMask = 0x001Fus

    /// <summary>The default-ignorable bit, <c>UPROPS_MASK_IGNORABLE</c>.</summary>
    [<Literal>]
    let Ignorable = 0x0020us

    /// <summary>The hidden bit, <c>UPROPS_MASK_HIDDEN</c>.</summary>
    [<Literal>]
    let Hidden = 0x0040us

    /// <summary>The grapheme-continuation bit, <c>UPROPS_MASK_CONTINUATION</c>.</summary>
    [<Literal>]
    let Continuation = 0x0080us

    /// <summary>The ZWJ bit, <c>UPROPS_MASK_Cf_ZWJ</c>.</summary>
    [<Literal>]
    let CfZwj = 0x0100us

    /// <summary>The ZWNJ bit, <c>UPROPS_MASK_Cf_ZWNJ</c>.</summary>
    [<Literal>]
    let CfZwnj = 0x0200us

    /// <summary>The variation-selector bit, <c>UPROPS_MASK_Cf_VS</c>.</summary>
    [<Literal>]
    let CfVariationSelector = 0x0400us

    /// <summary>The AAT-deleted bit, <c>UPROPS_MASK_Cf_AAT_DELETED</c>.</summary>
    [<Literal>]
    let CfAatDeleted = 0x0800us

    /// <summary>The low byte without the general-category field, <c>0xFF &amp; ~UPROPS_MASK_GEN_CAT</c>.</summary>
    [<Literal>]
    let private GenCatClearedLowByte = 0x00E0us

    /// <summary>The low byte of a property, <c>0xFF</c>.</summary>
    [<Literal>]
    let private LowByte = 0x00FFus

    /// <summary>Maps a general-category value back to its category.</summary>
    /// <param name="value">The packed value 0 to 29.</param>
    /// <returns>The category.</returns>
    let private categoryOfValue (value: uint16) : GeneralCategory =
        match value with
        | 0us -> GeneralCategory.Control | 1us -> GeneralCategory.Format | 2us -> GeneralCategory.Unassigned
        | 3us -> GeneralCategory.PrivateUse | 4us -> GeneralCategory.Surrogate | 5us -> GeneralCategory.LowercaseLetter
        | 6us -> GeneralCategory.ModifierLetter | 7us -> GeneralCategory.OtherLetter | 8us -> GeneralCategory.TitlecaseLetter
        | 9us -> GeneralCategory.UppercaseLetter | 10us -> GeneralCategory.SpacingMark | 11us -> GeneralCategory.EnclosingMark
        | 12us -> GeneralCategory.NonSpacingMark | 13us -> GeneralCategory.DecimalNumber | 14us -> GeneralCategory.LetterNumber
        | 15us -> GeneralCategory.OtherNumber | 16us -> GeneralCategory.ConnectPunctuation | 17us -> GeneralCategory.DashPunctuation
        | 18us -> GeneralCategory.ClosePunctuation | 19us -> GeneralCategory.FinalPunctuation | 20us -> GeneralCategory.InitialPunctuation
        | 21us -> GeneralCategory.OtherPunctuation | 22us -> GeneralCategory.OpenPunctuation | 23us -> GeneralCategory.CurrencySymbol
        | 24us -> GeneralCategory.ModifierSymbol | 25us -> GeneralCategory.MathSymbol | 26us -> GeneralCategory.OtherSymbol
        | 27us -> GeneralCategory.LineSeparator | 28us -> GeneralCategory.ParagraphSeparator | 29us -> GeneralCategory.SpaceSeparator
        | _ -> GeneralCategory.Unassigned

    /// <summary>Returns the numeric value of a space fallback type, the <c>space_t</c> enum order.</summary>
    /// <param name="space">The space type.</param>
    /// <returns>The value.</returns>
    let private spaceValue (space: SpaceFallback) : int =
        match space with
        | SpaceFallback.NotSpace -> 0 | SpaceFallback.SpaceEm -> 1 | SpaceFallback.SpaceEm2 -> 2
        | SpaceFallback.SpaceEm3 -> 3 | SpaceFallback.SpaceEm4 -> 4 | SpaceFallback.SpaceEm5 -> 5
        | SpaceFallback.SpaceEm6 -> 6 | SpaceFallback.SpaceEm16 -> 16 | SpaceFallback.Space4Em18 -> 17
        | SpaceFallback.Space -> 18 | SpaceFallback.SpaceFigure -> 19 | SpaceFallback.SpacePunctuation -> 20
        | SpaceFallback.SpaceNarrow -> 21

    /// <summary>Maps a space value back to its fallback type, the <c>space_t</c> enum order.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The space type.</returns>
    let private spaceOfValue (value: uint16) : SpaceFallback =
        match int value with
        | 0 -> SpaceFallback.NotSpace | 1 -> SpaceFallback.SpaceEm | 2 -> SpaceFallback.SpaceEm2
        | 3 -> SpaceFallback.SpaceEm3 | 4 -> SpaceFallback.SpaceEm4 | 5 -> SpaceFallback.SpaceEm5
        | 6 -> SpaceFallback.SpaceEm6 | 16 -> SpaceFallback.SpaceEm16 | 17 -> SpaceFallback.Space4Em18
        | 18 -> SpaceFallback.Space | 19 -> SpaceFallback.SpaceFigure | 20 -> SpaceFallback.SpacePunctuation
        | 21 -> SpaceFallback.SpaceNarrow | _ -> SpaceFallback.NotSpace

    /// <summary>Reads the general category, <c>_hb_glyph_info_get_general_category</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The category.</returns>
    let generalCategory (props: GlyphProps) : GeneralCategory =
        categoryOfValue (props.UnicodeProps &&& GenCatMask)

    /// <summary>Replaces the general category, clearing the top byte, <c>_hb_glyph_info_set_general_category</c>.</summary>
    /// <param name="gc">The category.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let setGeneralCategory (gc: GeneralCategory) (props: GlyphProps) : GlyphProps =
        { props with UnicodeProps = uint16 (categoryValue gc) ||| (props.UnicodeProps &&& GenCatClearedLowByte) }

    /// <summary>The mark categories as a bit mask over the category value, <c>FLAG_UNSAFE</c> applied to
    /// <c>HB_UNICODE_GENERAL_CATEGORY_IS_MARK</c> (hb-unicode.hh:405-409).</summary>
    [<Literal>]
    let private MarkCategoryMask = 0x1C00

    /// <summary>Whether a glyph is a Unicode mark, the direct bit test of <c>_hb_glyph_info_is_unicode_mark</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for spacing, enclosing and non-spacing marks.</returns>
    let isUnicodeMark (props: GlyphProps) : bool =
        (1 <<< int (props.UnicodeProps &&& GenCatMask)) &&& MarkCategoryMask <> 0

    /// <summary>Whether packed Unicode properties are those of a mark, <see cref="isUnicodeMark"/> on properties not held in a glyph.</summary>
    /// <param name="packed">The packed properties.</param>
    /// <returns>True for spacing, enclosing and non-spacing marks.</returns>
    let isMarkValue (packed: uint16) =
        (1 <<< int (packed &&& GenCatMask)) &&& MarkCategoryMask <> 0

    /// <summary>The non-spacing-mark category bit, value 12 of <c>HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK</c>.</summary>
    [<Literal>]
    let private NonSpacingMarkMask = 0x1000

    /// <summary>Whether a glyph is a non-spacing mark, the direct bit test behind
    /// <c>_hb_glyph_info_get_general_category</c> equal to NON_SPACING_MARK.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for non-spacing marks.</returns>
    let isNonSpacingMark (props: GlyphProps) =
        (1 <<< int (props.UnicodeProps &&& GenCatMask)) &&& NonSpacingMarkMask <> 0

    /// <summary>Reads the modified combining class, zero for non-marks, <c>_hb_glyph_info_get_modified_combining_class</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The class.</returns>
    let modifiedCombiningClass (props: GlyphProps) : byte =
        match isUnicodeMark props with
        | false -> 0uy
        | true -> byte (props.UnicodeProps >>> 8)

    /// <summary>Replaces the modified combining class of a mark, <c>_hb_glyph_info_set_modified_combining_class</c>.</summary>
    /// <param name="klass">The class.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let setModifiedCombiningClass (klass: byte) (props: GlyphProps) : GlyphProps =
        match isUnicodeMark props with
        | false -> props
        | true -> { props with UnicodeProps = uint16 (int klass <<< 8) ||| (props.UnicodeProps &&& LowByte) }

    /// <summary>Whether a glyph is a space separator, <c>_hb_glyph_info_is_unicode_space</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for Zs.</returns>
    let isUnicodeSpace (props: GlyphProps) : bool =
        generalCategory props = GeneralCategory.SpaceSeparator

    /// <summary>Whether packed Unicode properties are those of a space separator, <see cref="isUnicodeSpace"/> on properties not yet in a glyph.</summary>
    /// <param name="packed">The packed properties.</param>
    /// <returns>True for Zs.</returns>
    let isSpaceValue (packed: uint16) =
        categoryOfValue (packed &&& GenCatMask) = GeneralCategory.SpaceSeparator

    /// <summary>Reads the space fallback type, <c>_hb_glyph_info_get_unicode_space_fallback_type</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The space type, or <c>NotSpace</c> for non-spaces.</returns>
    let spaceFallback (props: GlyphProps) : SpaceFallback =
        match isUnicodeSpace props with
        | false -> SpaceFallback.NotSpace
        | true -> spaceOfValue (props.UnicodeProps >>> 8)

    /// <summary>Replaces the space fallback type of a space, <c>_hb_glyph_info_set_unicode_space_fallback_type</c>.</summary>
    /// <param name="space">The space type.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let setSpaceFallback (space: SpaceFallback) (props: GlyphProps) : GlyphProps =
        match isUnicodeSpace props with
        | false -> props
        | true -> { props with UnicodeProps = uint16 (spaceValue space <<< 8) ||| (props.UnicodeProps &&& LowByte) }

    /// <summary>Whether a glyph is a variation selector, <c>_hb_glyph_info_is_variation_selector</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the remembered Cf variation selectors.</returns>
    let isVariationSelector (props: GlyphProps) : bool =
        generalCategory props = GeneralCategory.Format && props.UnicodeProps &&& CfVariationSelector <> 0us

    /// <summary>Marks or restores a variation selector, <c>_hb_glyph_info_set_variation_selector</c>.</summary>
    /// <param name="customize">True to mark, false to restore the non-spacing-mark category.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let setVariationSelector (customize: bool) (props: GlyphProps) : GlyphProps =
        match customize with
        | true ->
            let formatted = setGeneralCategory GeneralCategory.Format props
            { formatted with UnicodeProps = formatted.UnicodeProps ||| CfVariationSelector }
        | false -> setGeneralCategory GeneralCategory.NonSpacingMark props

    /// <summary>Whether a glyph is default-ignorable and not substituted, <c>_hb_glyph_info_is_default_ignorable</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the unsubstituted ignorables.</returns>
    let isDefaultIgnorable (props: GlyphProps) : bool =
        props.UnicodeProps &&& Ignorable <> 0us && props.GlyphClass &&& GlyphPropsBits.Substituted = 0us

    /// <summary>Clears the default-ignorable bit, <c>_hb_glyph_info_clear_default_ignorable</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let clearDefaultIgnorable (props: GlyphProps) : GlyphProps =
        { props with UnicodeProps = props.UnicodeProps &&& ~~~Ignorable }

    /// <summary>Whether a glyph is hidden, <c>_hb_glyph_info_is_hidden</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the hidden Mongolian selectors, tags and CGJ.</returns>
    let isHidden (props: GlyphProps) : bool =
        props.UnicodeProps &&& Hidden <> 0us

    /// <summary>Unhides a glyph, <c>_hb_glyph_info_unhide</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let unhide (props: GlyphProps) : GlyphProps =
        { props with UnicodeProps = props.UnicodeProps &&& ~~~Hidden }

    /// <summary>Whether a glyph is a grapheme continuation, <c>_hb_glyph_info_is_continuation</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the continuation bit.</returns>
    let isContinuation (props: GlyphProps) : bool =
        props.UnicodeProps &&& Continuation <> 0us

    /// <summary>Marks a glyph a continuation, <c>_hb_glyph_info_set_continuation</c> (the scratch flag is added by the caller).</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let setContinuation (props: GlyphProps) : GlyphProps =
        { props with UnicodeProps = props.UnicodeProps ||| Continuation }

    /// <summary>Clears the continuation bit, <c>_hb_glyph_info_clear_continuation</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The updated properties.</returns>
    let clearContinuation (props: GlyphProps) : GlyphProps =
        { props with UnicodeProps = props.UnicodeProps &&& ~~~Continuation }

    /// <summary>Whether a glyph is a ZWNJ, <c>_hb_glyph_info_is_zwnj</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the ZWNJ Cf glyph.</returns>
    let isZwnj (props: GlyphProps) : bool =
        generalCategory props = GeneralCategory.Format && props.UnicodeProps &&& CfZwnj <> 0us

    /// <summary>Whether a glyph is a ZWJ, <c>_hb_glyph_info_is_zwj</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>True for the ZWJ Cf glyph.</returns>
    let isZwj (props: GlyphProps) : bool =
        generalCategory props = GeneralCategory.Format && props.UnicodeProps &&& CfZwj <> 0us
