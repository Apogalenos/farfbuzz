namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Hebrew shaper's hooks, <c>_hb_ot_shaper_hebrew</c> (src/hb-ot-shaper-hebrew.cc:34-208): the
/// presentation-form composition and the mark reordering.</summary>
[<RequireQualifiedAccess>]
module internal Hebrew =
    /// <summary>The letter alef, <c>0x05D0</c>.</summary>
    [<Literal>]
    let private Alef = 0x05D0u

    /// <summary>The letter tav, <c>0x05EA</c>.</summary>
    [<Literal>]
    let private Tav = 0x05EAu

    /// <summary>The letter yod, <c>0x05D9</c>.</summary>
    [<Literal>]
    let private Yod = 0x05D9u

    /// <summary>The letter vav, <c>0x05D5</c>.</summary>
    [<Literal>]
    let private Vav = 0x05D5u

    /// <summary>The letter shin, <c>0x05E9</c>.</summary>
    [<Literal>]
    let private Shin = 0x05E9u

    /// <summary>The letter bet, <c>0x05D1</c>.</summary>
    [<Literal>]
    let private Bet = 0x05D1u

    /// <summary>The letter kaf, <c>0x05DB</c>.</summary>
    [<Literal>]
    let private Kaf = 0x05DBu

    /// <summary>The letter pe, <c>0x05E4</c>.</summary>
    [<Literal>]
    let private Pe = 0x05E4u

    /// <summary>The Yiddish yod yod ligature letter, <c>0x05F2</c>.</summary>
    [<Literal>]
    let private YiddishYodYod = 0x05F2u

    /// <summary>The shin with shin dot presentation form, <c>0xFB2A</c>.</summary>
    [<Literal>]
    let private ShinWithShinDot = 0xFB2Au

    /// <summary>The shin with sin dot presentation form, <c>0xFB2B</c>.</summary>
    [<Literal>]
    let private ShinWithSinDot = 0xFB2Bu

    /// <summary>The shin with dagesh presentation form, <c>0xFB49</c>.</summary>
    [<Literal>]
    let private ShinWithDagesh = 0xFB49u

    /// <summary>The point hiriq, <c>0x05B4</c>.</summary>
    [<Literal>]
    let private Hiriq = 0x05B4u

    /// <summary>The point patah, <c>0x05B7</c>.</summary>
    [<Literal>]
    let private Patah = 0x05B7u

    /// <summary>The point qamats, <c>0x05B8</c>.</summary>
    [<Literal>]
    let private Qamats = 0x05B8u

    /// <summary>The point holam, <c>0x05B9</c>.</summary>
    [<Literal>]
    let private Holam = 0x05B9u

    /// <summary>The point dagesh, <c>0x05BC</c>.</summary>
    [<Literal>]
    let private Dagesh = 0x05BCu

    /// <summary>The point rafe, <c>0x05BF</c>.</summary>
    [<Literal>]
    let private Rafe = 0x05BFu

    /// <summary>The shin dot, <c>0x05C1</c>.</summary>
    [<Literal>]
    let private ShinDot = 0x05C1u

    /// <summary>The sin dot, <c>0x05C2</c>.</summary>
    [<Literal>]
    let private SinDot = 0x05C2u

    /// <summary>The modified combining class of the patah, <c>HB_MODIFIED_COMBINING_CLASS_CCC17</c>.</summary>
    [<Literal>]
    let private CccPatah = 20uy

    /// <summary>The modified combining class of the qamats, <c>HB_MODIFIED_COMBINING_CLASS_CCC18</c>.</summary>
    [<Literal>]
    let private CccQamats = 21uy

    /// <summary>The modified combining class of the sheva, <c>HB_MODIFIED_COMBINING_CLASS_CCC10</c>.</summary>
    [<Literal>]
    let private CccSheva = 22uy

    /// <summary>The modified combining class of the hiriq, <c>HB_MODIFIED_COMBINING_CLASS_CCC14</c>.</summary>
    [<Literal>]
    let private CccHiriq = 23uy

    /// <summary>The modified combining class of the meteg, <c>HB_MODIFIED_COMBINING_CLASS_CCC22</c>.</summary>
    [<Literal>]
    let private CccMeteg = 25uy

    /// <summary>The combining class of a mark below, <c>HB_UNICODE_COMBINING_CLASS_BELOW</c>.</summary>
    [<Literal>]
    let private CccBelow = 220uy

    /// <summary>The dagesh presentation forms of the letters U+05D0 to U+05EA, zero for a letter without one,
    /// <c>sDageshForms</c> (src/hb-ot-shaper-hebrew.cc:45-73).</summary>
    let private dageshForms : ImmutableArray<uint32> =
        Sealed.ofSpan (ReadOnlySpan [|
            0xFB30u; 0xFB31u; 0xFB32u; 0xFB33u; 0xFB34u; 0xFB35u; 0xFB36u; 0x0000u; 0xFB38u
            0xFB39u; 0xFB3Au; 0xFB3Bu; 0xFB3Cu; 0x0000u; 0xFB3Eu; 0x0000u; 0xFB40u; 0xFB41u
            0x0000u; 0xFB43u; 0xFB44u; 0x0000u; 0xFB46u; 0xFB47u; 0xFB48u; 0xFB49u; 0xFB4Au |])

    /// <summary>Composes a letter with a dagesh, <c>case 0x05BCu</c> of <c>compose_hebrew</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <returns>The presentation form, or none.</returns>
    let private withDagesh a =
        match a with
        | letter when letter >= Alef && letter <= Tav ->
            match dageshForms[int (letter - Alef)] with
            | 0u -> ValueNone
            | form -> ValueSome form
        | ShinWithShinDot -> ValueSome 0xFB2Cu
        | ShinWithSinDot -> ValueSome 0xFB2Du
        | _ -> ValueNone

    /// <summary>Composes a letter with a rafe, <c>case 0x05BFu</c> of <c>compose_hebrew</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <returns>The presentation form, or none.</returns>
    let private withRafe a =
        match a with
        | Bet -> ValueSome 0xFB4Cu
        | Kaf -> ValueSome 0xFB4Du
        | Pe -> ValueSome 0xFB4Eu
        | _ -> ValueNone

    /// <summary>Composes a letter with a shin dot or a sin dot, <c>case 0x05C1u</c> and <c>case 0x05C2u</c> of <c>compose_hebrew</c>.</summary>
    /// <param name="dotted">The form of a shin with that dot.</param>
    /// <param name="dottedWithDagesh">The form of a shin with dagesh and that dot.</param>
    /// <param name="a">The first code point.</param>
    /// <returns>The presentation form, or none.</returns>
    let private withDot dotted dottedWithDagesh a =
        match a with
        | Shin -> ValueSome dotted
        | ShinWithDagesh -> ValueSome dottedWithDagesh
        | _ -> ValueNone

    /// <summary>The Hebrew presentation forms that normalization excludes but old fonts want, the <c>switch (b)</c> of
    /// <c>compose_hebrew</c> (src/hb-ot-shaper-hebrew.cc:84-157).</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The presentation form, or none.</returns>
    let private presentationForm a b =
        match b with
        | Hiriq -> if a = Yod then ValueSome 0xFB1Du else ValueNone
        | Patah ->
            match a with
            | YiddishYodYod -> ValueSome 0xFB1Fu
            | Alef -> ValueSome 0xFB2Eu
            | _ -> ValueNone
        | Qamats -> if a = Alef then ValueSome 0xFB2Fu else ValueNone
        | Holam -> if a = Vav then ValueSome 0xFB4Bu else ValueNone
        | Dagesh -> withDagesh a
        | Rafe -> withRafe a
        | ShinDot -> withDot ShinWithShinDot 0xFB2Cu a
        | SinDot -> withDot ShinWithSinDot 0xFB2Du a
        | _ -> ValueNone

    /// <summary>Composes two code points for the Hebrew shaper, <c>compose_hebrew</c>: Unicode composition first, then the
    /// presentation forms unless the plan positions marks with GPOS <c>mark</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let compose (plan: OtShapePlan) a b =
        match Unicode.compose a b with
        | ValueSome composed -> ValueSome composed
        | ValueNone ->
            match plan.HasGposMark with
            | true -> ValueNone
            | false -> presentationForm a b

    /// <summary>Composes two code points as <see cref="compose"/> does, Unicode composition through replaced Unicode functions.</summary>
    /// <param name="unicode">The functions.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let composeWith (unicode: UnicodeFuncs) (plan: OtShapePlan) a b =
        match unicode.Compose a b with
        | ValueSome composed -> ValueSome composed
        | ValueNone ->
            match plan.HasGposMark with
            | true -> ValueNone
            | false -> presentationForm a b

    /// <summary>Finds the first mark triple that needs swapping, the <c>for</c> loop of <c>reorder_marks_hebrew</c>:
    /// a patah or qamats, then a sheva or hiriq, then a meteg or a mark below.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The index of the third mark.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The index of the third mark, or -1 when no triple matches.</returns>
    [<TailCall>]
    let rec private findTriple (props: ReadOnlySpan<GlyphProps>) index finish =
        match index >= finish with
        | true -> -1
        | false ->
            let c0 = UnicodeProps.modifiedCombiningClass props[index - 2]
            let c1 = UnicodeProps.modifiedCombiningClass props[index - 1]
            let c2 = UnicodeProps.modifiedCombiningClass props[index]
            match (c0 = CccPatah || c0 = CccQamats) && (c1 = CccSheva || c1 = CccHiriq) && (c2 = CccMeteg || c2 = CccBelow) with
            | true -> index
            | false -> findTriple props (index + 1) finish

    /// <summary>The state of the swap pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SwapState =
        | SwapMerge
        | SwapExchange
        | SwapDone

    /// <summary>The pass that merges the clusters of two adjacent marks and swaps them.</summary>
    [<Struct>]
    type private SwapPass =
        val Index: int

        /// <summary>Creates the pass.</summary>
        /// <param name="index">The index of the second of the two marks.</param>
        new (index) = { Index = index }

        interface IRunPass<SwapState> with
            member this.Step (_, state) =
                match state with
                | SwapMerge -> struct (RunEdit.MergeClusters (this.Index - 1, this.Index + 1), SwapExchange)
                | SwapExchange -> struct (RunEdit.Rearrange (this.Index - 1, this.Index + 1, 1, 1, false, false), SwapDone)
                | SwapDone -> struct (RunEdit.Finish, SwapDone)

    /// <summary>Reorders the marks of a sorted mark run, <c>reorder_marks_hebrew</c>: the first patah or qamats, sheva or
    /// hiriq, meteg or below-mark triple has its last two marks swapped and their clusters merged.</summary>
    /// <param name="run">The run, its mark run sorted by modified combining class.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The run with the marks reordered.</returns>
    let reorderMarks (run: GlyphRun) start finish =
        match findTriple (run.Props.AsSpan()) (start + 2) finish with
        | -1 -> run
        | index -> GlyphRun.runPass (SwapPass(index)) SwapMerge run

    /// <summary>The edits <see cref="reorderMarks"/> makes to a sorted mark run, for a caller that applies edits to its own working
    /// copy: the clusters of the last two marks of the first matching triple are merged and the two marks swapped.</summary>
    /// <param name="props">The glyph properties, the mark run sorted by modified combining class.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The edits, none when no triple matches.</returns>
    let reorderEdits (props: ReadOnlySpan<GlyphProps>) start finish =
        match findTriple props (start + 2) finish with
        | -1 -> ImmutableArray<RunEdit>.Empty
        | index ->
            Sealed.ofSpan (ReadOnlySpan [|
                RunEdit.MergeClusters (index - 1, index + 1)
                RunEdit.Rearrange (index - 1, index + 1, 1, 1, false, false)
            |])
