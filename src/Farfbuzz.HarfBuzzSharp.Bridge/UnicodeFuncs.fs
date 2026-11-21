namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open Farfbuzz

/// <summary>The Unicode functions of a HarfBuzzSharp <c>UnicodeFunctions</c> for the shell: the default queries, and the core functions a buffer's functions become (hb-unicode.cc, hb-ucd.cc).</summary>
[<AbstractClass; Sealed>]
type internal UnicodeQueries =
    /// <summary>The general categories in the order of <c>hb_unicode_general_category_t</c>.</summary>
    static member private Categories =
        [| GeneralCategory.Control; GeneralCategory.Format; GeneralCategory.Unassigned; GeneralCategory.PrivateUse; GeneralCategory.Surrogate
           GeneralCategory.LowercaseLetter; GeneralCategory.ModifierLetter; GeneralCategory.OtherLetter; GeneralCategory.TitlecaseLetter
           GeneralCategory.UppercaseLetter; GeneralCategory.SpacingMark; GeneralCategory.EnclosingMark; GeneralCategory.NonSpacingMark
           GeneralCategory.DecimalNumber; GeneralCategory.LetterNumber; GeneralCategory.OtherNumber; GeneralCategory.ConnectPunctuation
           GeneralCategory.DashPunctuation; GeneralCategory.ClosePunctuation; GeneralCategory.FinalPunctuation; GeneralCategory.InitialPunctuation
           GeneralCategory.OtherPunctuation; GeneralCategory.OpenPunctuation; GeneralCategory.CurrencySymbol; GeneralCategory.ModifierSymbol
           GeneralCategory.MathSymbol; GeneralCategory.OtherSymbol; GeneralCategory.LineSeparator; GeneralCategory.ParagraphSeparator
           GeneralCategory.SpaceSeparator |]

    /// <summary>The canonical combining class of the default functions, <c>hb_ucd_combining_class</c>.</summary>
    /// <param name="unicode">The code point.</param>
    /// <returns>The class.</returns>
    static member CombiningClass (unicode: uint32) =
        int (Unicode.combiningClass unicode)

    /// <summary>The general category of the default functions, <c>hb_ucd_general_category</c>.</summary>
    /// <param name="unicode">The code point.</param>
    /// <returns>The <c>hb_unicode_general_category_t</c> value.</returns>
    static member GeneralCategory (unicode: uint32) =
        Array.IndexOf(UnicodeQueries.Categories, Unicode.generalCategory unicode)

    /// <summary>The mirrored code point of the default functions, <c>hb_ucd_mirroring</c>.</summary>
    /// <param name="unicode">The code point.</param>
    /// <returns>The mirrored code point, or the code point itself.</returns>
    static member Mirroring (unicode: uint32) =
        Unicode.mirroring unicode

    /// <summary>The script of the default functions, <c>hb_ucd_script</c>.</summary>
    /// <param name="unicode">The code point.</param>
    /// <returns>The script tag value.</returns>
    static member Script (unicode: uint32) =
        uint32 (Unicode.script unicode)

    /// <summary>The composition of the default functions, <c>hb_ucd_compose</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>Whether they compose, and the composite.</returns>
    static member Compose (a: uint32, b: uint32) =
        match Unicode.compose a b with
        | ValueSome composed -> struct (true, composed)
        | ValueNone -> struct (false, 0u)

    /// <summary>The decomposition of the default functions, <c>hb_ucd_decompose</c>.</summary>
    /// <param name="ab">The code point.</param>
    /// <returns>Whether it decomposes, and the two parts.</returns>
    static member Decompose (ab: uint32) =
        match Unicode.decompose ab with
        | ValueSome (struct (a, b)) -> struct (true, a, b)
        | ValueNone -> struct (false, 0u, 0u)

    /// <summary>The core functions the replaced functions of a buffer make, a slot with no function being the default one (<c>hb_ucd_*</c>).</summary>
    /// <param name="combining">The combining class function, or null.</param>
    /// <param name="category">The general category function, or null.</param>
    /// <param name="mirroring">The mirroring function, or null.</param>
    /// <param name="script">The script function, or null.</param>
    /// <param name="compose">The composition function, or null.</param>
    /// <param name="decompose">The decomposition function, or null.</param>
    /// <returns>The functions.</returns>
    static member Build
            (combining: Func<uint32, int> | null,
             category: Func<uint32, int> | null,
             mirroring: Func<uint32, uint32> | null,
             script: Func<uint32, uint32> | null,
             compose: Func<uint32, uint32, struct (bool * uint32)> | null,
             decompose: Func<uint32, struct (bool * uint32 * uint32)> | null) =
        let categories = UnicodeQueries.Categories
        { UnicodeFuncs.CombiningClass =
            match combining with
            | null -> Unicode.combiningClass
            | answer -> fun unicode -> byte (answer.Invoke unicode)
          GeneralCategory =
            match category with
            | null -> Unicode.generalCategory
            | answer -> fun unicode -> categories[Math.Clamp(answer.Invoke unicode, 0, categories.Length - 1)]
          Mirroring =
            match mirroring with
            | null -> Unicode.mirroring
            | answer -> fun unicode -> answer.Invoke unicode
          Script =
            match script with
            | null -> Unicode.script
            | answer -> fun unicode -> LanguagePrimitives.UInt32WithMeasure (answer.Invoke unicode)
          Compose =
            match compose with
            | null -> Unicode.compose
            | answer ->
                fun a b ->
                    match answer.Invoke (a, b) with
                    | struct (true, composed) -> ValueSome composed
                    | struct (false, _) -> ValueNone
          Decompose =
            match decompose with
            | null -> Unicode.decompose
            | answer ->
                fun ab ->
                    match answer.Invoke ab with
                    | struct (true, a, b) -> ValueSome (struct (a, b))
                    | struct (false, _, _) -> ValueNone }
