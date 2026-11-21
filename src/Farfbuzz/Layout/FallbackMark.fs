namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The fallback mark recategorization and positioning of <c>hb-ot-shape-fallback.cc</c>.</summary>
[<RequireQualifiedAccess>]
module internal FallbackMark =
    /// <summary>The attached-below-left combining class, <c>HB_UNICODE_COMBINING_CLASS_ATTACHED_BELOW_LEFT</c>.</summary>
    [<Literal>]
    let private AttachedBelowLeft = 200u

    /// <summary>The attached-below combining class, <c>HB_UNICODE_COMBINING_CLASS_ATTACHED_BELOW</c>.</summary>
    [<Literal>]
    let private AttachedBelow = 202u

    /// <summary>The attached-above combining class, <c>HB_UNICODE_COMBINING_CLASS_ATTACHED_ABOVE</c>.</summary>
    [<Literal>]
    let private AttachedAbove = 214u

    /// <summary>The attached-above-right combining class, <c>HB_UNICODE_COMBINING_CLASS_ATTACHED_ABOVE_RIGHT</c>.</summary>
    [<Literal>]
    let private AttachedAboveRight = 216u

    /// <summary>The below-left combining class, <c>HB_UNICODE_COMBINING_CLASS_BELOW_LEFT</c>.</summary>
    [<Literal>]
    let private BelowLeft = 218u

    /// <summary>The below combining class, <c>HB_UNICODE_COMBINING_CLASS_BELOW</c>.</summary>
    [<Literal>]
    let private Below = 220u

    /// <summary>The below-right combining class, <c>HB_UNICODE_COMBINING_CLASS_BELOW_RIGHT</c>.</summary>
    [<Literal>]
    let private BelowRight = 222u

    /// <summary>The above-left combining class, <c>HB_UNICODE_COMBINING_CLASS_ABOVE_LEFT</c>.</summary>
    [<Literal>]
    let private AboveLeft = 228u

    /// <summary>The above combining class, <c>HB_UNICODE_COMBINING_CLASS_ABOVE</c>.</summary>
    [<Literal>]
    let private Above = 230u

    /// <summary>The above-right combining class, <c>HB_UNICODE_COMBINING_CLASS_ABOVE_RIGHT</c>.</summary>
    [<Literal>]
    let private AboveRight = 232u

    /// <summary>The double-below combining class, <c>HB_UNICODE_COMBINING_CLASS_DOUBLE_BELOW</c>.</summary>
    [<Literal>]
    let private DoubleBelow = 233u

    /// <summary>The double-above combining class, <c>HB_UNICODE_COMBINING_CLASS_DOUBLE_ABOVE</c>.</summary>
    [<Literal>]
    let private DoubleAbove = 234u

    /// <summary>The modified class of sheva, <c>HB_MODIFIED_COMBINING_CLASS_CCC10</c>.</summary>
    [<Literal>]
    let private ModifiedCcc10 = 22u

    /// <summary>The modified class of hataf segol, <c>HB_MODIFIED_COMBINING_CLASS_CCC11</c>.</summary>
    [<Literal>]
    let private ModifiedCcc11 = 15u

    /// <summary>The modified class of hataf patah, <c>HB_MODIFIED_COMBINING_CLASS_CCC12</c>.</summary>
    [<Literal>]
    let private ModifiedCcc12 = 16u

    /// <summary>The modified class of hataf qamats, <c>HB_MODIFIED_COMBINING_CLASS_CCC13</c>.</summary>
    [<Literal>]
    let private ModifiedCcc13 = 17u

    /// <summary>The modified class of hiriq, <c>HB_MODIFIED_COMBINING_CLASS_CCC14</c>.</summary>
    [<Literal>]
    let private ModifiedCcc14 = 23u

    /// <summary>The modified class of tsere, <c>HB_MODIFIED_COMBINING_CLASS_CCC15</c>.</summary>
    [<Literal>]
    let private ModifiedCcc15 = 18u

    /// <summary>The modified class of segol, <c>HB_MODIFIED_COMBINING_CLASS_CCC16</c>.</summary>
    [<Literal>]
    let private ModifiedCcc16 = 19u

    /// <summary>The modified class of patah, <c>HB_MODIFIED_COMBINING_CLASS_CCC17</c>.</summary>
    [<Literal>]
    let private ModifiedCcc17 = 20u

    /// <summary>The modified class of qamats, <c>HB_MODIFIED_COMBINING_CLASS_CCC18</c>.</summary>
    [<Literal>]
    let private ModifiedCcc18 = 21u

    /// <summary>The modified class of holam, <c>HB_MODIFIED_COMBINING_CLASS_CCC19</c>.</summary>
    [<Literal>]
    let private ModifiedCcc19 = 14u

    /// <summary>The modified class of qubuts, <c>HB_MODIFIED_COMBINING_CLASS_CCC20</c>.</summary>
    [<Literal>]
    let private ModifiedCcc20 = 24u

    /// <summary>The modified class of dagesh, <c>HB_MODIFIED_COMBINING_CLASS_CCC21</c>.</summary>
    [<Literal>]
    let private ModifiedCcc21 = 12u

    /// <summary>The modified class of meteg, <c>HB_MODIFIED_COMBINING_CLASS_CCC22</c>.</summary>
    [<Literal>]
    let private ModifiedCcc22 = 25u

    /// <summary>The modified class of rafe, <c>HB_MODIFIED_COMBINING_CLASS_CCC23</c>.</summary>
    [<Literal>]
    let private ModifiedCcc23 = 13u

    /// <summary>The modified class of shin dot, <c>HB_MODIFIED_COMBINING_CLASS_CCC24</c>.</summary>
    [<Literal>]
    let private ModifiedCcc24 = 10u

    /// <summary>The modified class of sin dot, <c>HB_MODIFIED_COMBINING_CLASS_CCC25</c>.</summary>
    [<Literal>]
    let private ModifiedCcc25 = 11u

    /// <summary>The modified class of point varika, <c>HB_MODIFIED_COMBINING_CLASS_CCC26</c>.</summary>
    [<Literal>]
    let private ModifiedCcc26 = 26u

    /// <summary>The modified class of fathatan, <c>HB_MODIFIED_COMBINING_CLASS_CCC27</c>.</summary>
    [<Literal>]
    let private ModifiedCcc27 = 28u

    /// <summary>The modified class of dammatan, <c>HB_MODIFIED_COMBINING_CLASS_CCC28</c>.</summary>
    [<Literal>]
    let private ModifiedCcc28 = 29u

    /// <summary>The modified class of kasratan, <c>HB_MODIFIED_COMBINING_CLASS_CCC29</c>.</summary>
    [<Literal>]
    let private ModifiedCcc29 = 30u

    /// <summary>The modified class of fatha, <c>HB_MODIFIED_COMBINING_CLASS_CCC30</c>.</summary>
    [<Literal>]
    let private ModifiedCcc30 = 31u

    /// <summary>The modified class of damma, <c>HB_MODIFIED_COMBINING_CLASS_CCC31</c>.</summary>
    [<Literal>]
    let private ModifiedCcc31 = 32u

    /// <summary>The modified class of kasra, <c>HB_MODIFIED_COMBINING_CLASS_CCC32</c>.</summary>
    [<Literal>]
    let private ModifiedCcc32 = 33u

    /// <summary>The modified class of shadda, <c>HB_MODIFIED_COMBINING_CLASS_CCC33</c>.</summary>
    [<Literal>]
    let private ModifiedCcc33 = 27u

    /// <summary>The modified class of sukun, <c>HB_MODIFIED_COMBINING_CLASS_CCC34</c>.</summary>
    [<Literal>]
    let private ModifiedCcc34 = 34u

    /// <summary>The modified class of superscript alef, <c>HB_MODIFIED_COMBINING_CLASS_CCC35</c>.</summary>
    [<Literal>]
    let private ModifiedCcc35 = 35u

    /// <summary>The modified class of superscript alaph, <c>HB_MODIFIED_COMBINING_CLASS_CCC36</c>.</summary>
    [<Literal>]
    let private ModifiedCcc36 = 36u

    /// <summary>The modified class of sara u, <c>HB_MODIFIED_COMBINING_CLASS_CCC103</c>.</summary>
    [<Literal>]
    let private ModifiedCcc103 = 3u

    /// <summary>The modified class of mai ek, <c>HB_MODIFIED_COMBINING_CLASS_CCC107</c>.</summary>
    [<Literal>]
    let private ModifiedCcc107 = 107u

    /// <summary>The modified class of Lao sign u, <c>HB_MODIFIED_COMBINING_CLASS_CCC118</c>.</summary>
    [<Literal>]
    let private ModifiedCcc118 = 118u

    /// <summary>The modified class of Lao mai, <c>HB_MODIFIED_COMBINING_CLASS_CCC122</c>.</summary>
    [<Literal>]
    let private ModifiedCcc122 = 122u

    /// <summary>The modified class of Tibetan sign aa, <c>HB_MODIFIED_COMBINING_CLASS_CCC129</c>.</summary>
    [<Literal>]
    let private ModifiedCcc129 = 129u

    /// <summary>The modified class of Tibetan sign i, <c>HB_MODIFIED_COMBINING_CLASS_CCC130</c>.</summary>
    [<Literal>]
    let private ModifiedCcc130 = 132u

    /// <summary>The modified class of Tibetan sign u, <c>HB_MODIFIED_COMBINING_CLASS_CCC132</c>.</summary>
    [<Literal>]
    let private ModifiedCcc132 = 131u

    /// <summary>Clamps an int64 into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32 (value: int64) : int =
        int (Math.Clamp(value, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Adds two positions, saturating instead of overflowing, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturated sum.</returns>
    let private saturateAdd (a: int) (b: int) : int =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Subtracts two positions, saturating instead of overflowing, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The saturated difference.</returns>
    let private saturateSub (a: int) (b: int) : int =
        int (Math.Clamp(int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Negates a position, saturating <c>Int32.MinValue</c> to <c>Int32.MaxValue</c>, <c>hb_saturate_neg</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The saturated negation.</returns>
    let private saturateNeg (value: int) : int =
        int (Math.Clamp(-(int64 value), int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>The Thai and Lao per-character classes of <c>recategorize_combining_class</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="klass">The modified combining class.</param>
    /// <returns>The class with the Thai or Lao special case applied.</returns>
    let private thaiLao (u: uint32) (klass: uint32) : uint32 =
        match u &&& ~~~0xFFu = 0x0E00u with
        | false -> klass
        | true ->
            match klass = 0u with
            | true ->
                match u with
                | 0x0E31u | 0x0E34u | 0x0E35u | 0x0E36u | 0x0E37u | 0x0E47u | 0x0E4Cu | 0x0E4Du | 0x0E4Eu -> AboveRight
                | 0x0EB1u | 0x0EB4u | 0x0EB5u | 0x0EB6u | 0x0EB7u | 0x0EBBu | 0x0ECCu | 0x0ECDu -> Above
                | 0x0EBCu -> Below
                | _ -> klass
            | false ->
                match u = 0x0E3Au with
                | true -> BelowRight
                | false -> klass

    /// <summary>The Hebrew, Arabic, Syriac, Thai, Lao and Tibetan class table of <c>recategorize_combining_class</c>.</summary>
    /// <param name="klass">The modified combining class, below 200.</param>
    /// <returns>The recategorized class.</returns>
    let private classify (klass: uint32) : uint32 =
        match klass with
        | ModifiedCcc10 | ModifiedCcc11 | ModifiedCcc12 | ModifiedCcc13 | ModifiedCcc14 | ModifiedCcc15
        | ModifiedCcc16 | ModifiedCcc17 | ModifiedCcc18 | ModifiedCcc20 | ModifiedCcc22 -> Below
        | ModifiedCcc23 -> AttachedAbove
        | ModifiedCcc24 -> AboveRight
        | ModifiedCcc25 | ModifiedCcc19 -> AboveLeft
        | ModifiedCcc26 -> Above
        | ModifiedCcc27 | ModifiedCcc28 | ModifiedCcc30 | ModifiedCcc31 | ModifiedCcc33 | ModifiedCcc34
        | ModifiedCcc35 | ModifiedCcc36 -> Above
        | ModifiedCcc29 | ModifiedCcc32 -> Below
        | ModifiedCcc103 -> BelowRight
        | ModifiedCcc107 -> AboveRight
        | ModifiedCcc118 -> Below
        | ModifiedCcc122 -> Above
        | ModifiedCcc129 -> Below
        | ModifiedCcc130 -> Above
        | ModifiedCcc132 -> Below
        | other -> other

    /// <summary>Recategorizes one mark's modified combining class, <c>recategorize_combining_class</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="klass">The modified combining class.</param>
    /// <returns>The recategorized class.</returns>
    let recategorizeCombiningClass (u: uint32) (klass: uint32) : uint32 =
        match klass >= 200u with
        | true -> klass
        | false -> classify (thaiLao u klass)

    /// <summary>Recategorizes one glyph's modified combining class when it is a non-spacing mark.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The properties with the recategorized class.</returns>
    let private recategorizeProp (u: uint32) (props: GlyphProps) : GlyphProps =
        match UnicodeProps.isNonSpacingMark props with
        | false -> props
        | true ->
            let klass = uint32 (UnicodeProps.modifiedCombiningClass props)
            UnicodeProps.setModifiedCombiningClass (byte (recategorizeCombiningClass u klass)) props

    /// <summary>Whether any glyph's modified combining class would change through recategorization.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph index.</param>
    /// <returns>True when at least one glyph changes.</returns>
    [<TailCall>]
    let rec private needsRecategorization (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) : bool =
        match index >= infos.Length with
        | true -> false
        | false ->
            match UnicodeProps.isNonSpacingMark props[index] with
            | false -> needsRecategorization infos props (index + 1)
            | true ->
                let klass = uint32 (UnicodeProps.modifiedCombiningClass props[index])
                match recategorizeCombiningClass infos[index].Codepoint klass with
                | same when same = klass -> needsRecategorization infos props (index + 1)
                | _ -> true

    /// <summary>Recategorizes the non-spacing marks of a run, <c>_hb_ot_shape_fallback_mark_position_recategorize_marks</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the marks recategorized, the same instance when nothing changes.</returns>
    let recategorizeMarks (run: GlyphRun) : GlyphRun =
        match needsRecategorization (run.Infos.AsSpan()) (run.Props.AsSpan()) 0 with
        | false -> run
        | true ->
            let props =
                Sealed.mapi (fun index (props: GlyphProps) -> recategorizeProp run.Infos[index].Codepoint props) (run.Props.AsSpan())
            { run with Props = props }

    /// <summary>The recategorised properties of one glyph, <see cref="recategorizeMarks"/> for the properties of a working set.</summary>
    [<Struct>]
    type private RecategorizeMapper =
        val Infos: ImmutableArray<GlyphInfo>

        /// <summary>Creates the mapper.</summary>
        /// <param name="infos">The glyphs, whose code points select the classes.</param>
        new (infos) = { Infos = infos }

        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) = recategorizeProp this.Infos[index].Codepoint items[index]

    /// <summary>Recategorizes the non-spacing marks of a working set whose arrays the caller made in this call and shared with no one, as
    /// <see cref="recategorizeMarks"/> does, in its properties.</summary>
    /// <param name="set">The working set.</param>
    /// <returns>The set with the marks recategorized.</returns>
    let recategorizeMarksSet (set: WorkingSet) =
        match needsRecategorization (set.Infos.AsSpan()) (set.Props.AsSpan()) 0 with
        | false -> set
        | true ->
            Sealed.mapOwned (RecategorizeMapper(set.Infos)) set.Props
            set

    /// <summary>Center-aligns a mark over its base, the default X case of <c>position_mark</c>.</summary>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The mark's X offset.</returns>
    let private centerAlign (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : int =
        clampToInt32 (int64 baseExtents.XBearing + (int64 baseExtents.Width - int64 markExtents.Width) / 2L - int64 markExtents.XBearing)

    /// <summary>The X positioning of <c>position_mark</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <param name="klass">The combining class.</param>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The mark's X offset.</returns>
    let private xPosition (direction: Direction) (klass: uint32) (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : int =
        match klass with
        | DoubleBelow | DoubleAbove ->
            match direction with
            | Direction.Ltr -> clampToInt32 (int64 baseExtents.XBearing + int64 baseExtents.Width - int64 (markExtents.Width / 2) - int64 markExtents.XBearing)
            | Direction.Rtl -> clampToInt32 (int64 baseExtents.XBearing - int64 (markExtents.Width / 2) - int64 markExtents.XBearing)
            | Direction.Ttb | Direction.Btt | Direction.Invalid -> centerAlign baseExtents markExtents
        | AttachedBelow | AttachedAbove | Below | Above -> centerAlign baseExtents markExtents
        | AttachedBelowLeft | BelowLeft | AboveLeft -> saturateSub baseExtents.XBearing markExtents.XBearing
        | AttachedAboveRight | BelowRight | AboveRight ->
            clampToInt32 (int64 baseExtents.XBearing + int64 baseExtents.Width - int64 markExtents.Width - int64 markExtents.XBearing)
        | _ -> centerAlign baseExtents markExtents

    /// <summary>The attached-below Y positioning of <c>position_mark</c>.</summary>
    /// <param name="yGap">The mark gap, <c>y_scale / 16</c>.</param>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The updated base extents and the mark's Y offset.</returns>
    let private attachedBelow (yGap: int) (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : struct (GlyphExtents * int) =
        let yOffset = clampToInt32 (int64 baseExtents.YBearing + int64 baseExtents.Height - int64 markExtents.YBearing)
        match (yGap > 0) = (yOffset > 0) with
        | true -> struct ({ baseExtents with Height = saturateAdd (saturateSub baseExtents.Height yOffset) markExtents.Height }, 0)
        | false -> struct ({ baseExtents with Height = saturateAdd baseExtents.Height markExtents.Height }, yOffset)

    /// <summary>The attached-above Y positioning of <c>position_mark</c>.</summary>
    /// <param name="yGap">The mark gap, <c>y_scale / 16</c>.</param>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The updated base extents and the mark's Y offset.</returns>
    let private attachedAbove (yGap: int) (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : struct (GlyphExtents * int) =
        let yOffset = clampToInt32 (int64 baseExtents.YBearing - int64 markExtents.YBearing - int64 markExtents.Height)
        match (yGap > 0) <> (yOffset > 0) with
        | true ->
            let correction = -(yOffset / 2)
            struct ({ baseExtents with YBearing = saturateSub (saturateAdd baseExtents.YBearing correction) markExtents.Height; Height = saturateAdd (saturateSub baseExtents.Height correction) markExtents.Height }, saturateAdd yOffset correction)
        | false -> struct ({ baseExtents with YBearing = saturateSub baseExtents.YBearing markExtents.Height; Height = saturateAdd baseExtents.Height markExtents.Height }, yOffset)

    /// <summary>The Y positioning of <c>position_mark</c>.</summary>
    /// <param name="yGap">The mark gap, <c>y_scale / 16</c>.</param>
    /// <param name="klass">The combining class.</param>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The updated base extents and the mark's Y offset.</returns>
    let private yPosition (yGap: int) (klass: uint32) (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : struct (GlyphExtents * int) =
        match klass with
        | DoubleBelow | BelowLeft | Below | BelowRight ->
            attachedBelow yGap { baseExtents with Height = saturateSub baseExtents.Height yGap } markExtents
        | AttachedBelowLeft | AttachedBelow -> attachedBelow yGap baseExtents markExtents
        | DoubleAbove | AboveLeft | Above | AboveRight ->
            attachedAbove yGap { baseExtents with YBearing = saturateAdd baseExtents.YBearing yGap; Height = saturateSub baseExtents.Height yGap } markExtents
        | AttachedAbove | AttachedAboveRight -> attachedAbove yGap baseExtents markExtents
        | _ -> struct (baseExtents, 0)

    /// <summary>Positions one mark over its base, <c>position_mark</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <param name="yGap">The mark gap, <c>y_scale / 16</c>.</param>
    /// <param name="klass">The combining class.</param>
    /// <param name="baseExtents">The base extents.</param>
    /// <param name="markExtents">The mark extents.</param>
    /// <returns>The updated base extents, the mark's X offset and its Y offset.</returns>
    let private positionMark (direction: Direction) (yGap: int) (klass: uint32) (baseExtents: GlyphExtents) (markExtents: GlyphExtents) : struct (GlyphExtents * int * int) =
        let xOffset = xPosition direction klass baseExtents markExtents
        let struct (baseExtents, yOffset) = yPosition yGap klass baseExtents markExtents
        struct (baseExtents, xOffset, yOffset)

    /// <summary>Whether a glyph trails a base: a mark, a hidden glyph or a default ignorable.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True for a trailing glyph.</returns>
    let private isTrailing (props: GlyphProps) : bool =
        UnicodeProps.isUnicodeMark props || UnicodeProps.isHidden props || UnicodeProps.isDefaultIgnorable props

    /// <summary>Finds the end of a base's mark run, <c>position_cluster_impl</c>'s inner scan.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next index after the base.</param>
    /// <returns>The first index that is not a mark, hidden or default ignorable.</returns>
    [<TailCall>]
    let rec private markRunEnd (props: ReadOnlySpan<GlyphProps>) (index: int) : int =
        match index < props.Length && isTrailing props[index] with
        | true -> markRunEnd props (index + 1)
        | false -> index

    /// <summary>Whether the run has a base followed by a mark run, so positioning would change it.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph index.</param>
    /// <returns>True when at least one base has a trailing mark run.</returns>
    [<TailCall>]
    let rec private hasMarkCluster (props: ReadOnlySpan<GlyphProps>) (index: int) : bool =
        match index + 1 >= props.Length with
        | true -> false
        | false ->
            match (not (UnicodeProps.isUnicodeMark props[index])) && isTrailing props[index + 1] with
            | true -> true
            | false -> hasMarkCluster props (index + 1)

    /// <summary>Whether a range holds a mark with a non-zero combining class.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first index.</param>
    /// <param name="finish">The exclusive end.</param>
    /// <returns>True when at least one glyph in the range is a positioned mark.</returns>
    [<TailCall>]
    let rec private hasMarkInRange (props: ReadOnlySpan<GlyphProps>) (index: int) (finish: int) : bool =
        match index >= finish with
        | true -> false
        | false ->
            match UnicodeProps.modifiedCombiningClass props[index] <> 0uy with
            | true -> true
            | false -> hasMarkInRange props (index + 1) finish

    /// <summary>Whether a range holds a glyph the pass changes once the base extents are read: a mark with a non-zero combining class, which
    /// is positioned, or a non-spacing mark, which is zeroed when the extents of its base are unavailable.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first index.</param>
    /// <param name="finish">The exclusive end.</param>
    /// <returns>True when at least one glyph in the range can be positioned or zeroed.</returns>
    [<TailCall>]
    let rec private hasTouchedMarkInRange (props: ReadOnlySpan<GlyphProps>) (index: int) (finish: int) =
        match index >= finish with
        | true -> false
        | false ->
            match UnicodeProps.modifiedCombiningClass props[index] <> 0uy || UnicodeProps.generalCategory props[index] = GeneralCategory.NonSpacingMark with
            | true -> true
            | false -> hasTouchedMarkInRange props (index + 1) finish

    /// <summary>Whether the run has a base followed by a mark the pass positions or zeroes, so that the base's extents are read.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph index.</param>
    /// <returns>True when at least one base has a mark that can be positioned or zeroed after it.</returns>
    [<TailCall>]
    let rec private hasMarkToPosition (props: ReadOnlySpan<GlyphProps>) (index: int) : bool =
        match index >= props.Length with
        | true -> false
        | false ->
            match UnicodeProps.isUnicodeMark props[index] with
            | true -> hasMarkToPosition props (index + 1)
            | false ->
                let finish = markRunEnd props (index + 1)
                match hasTouchedMarkInRange props (index + 1) finish with
                | true -> true
                | false -> hasMarkToPosition props finish

    /// <summary>The outline source a face's glyph extents cannot answer, when any.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The unported source, or none when the glyf extents answer.</returns>
    let private unportedSource (font: Font) : OutlineSource voption =
        match Extents.source font with
        | ValueSome source -> ValueSome source
        | ValueNone ->
            match font.IsSynthetic with
            | true -> ValueSome SyntheticOutlines
            | false -> ValueNone

    /// <summary>The state of positioning the marks around one base.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private AroundState =
        { Base: int
          Finish: int
          BaseExtents: GlyphExtents
          ComponentExtents: GlyphExtents
          ClusterExtents: GlyphExtents
          LigId: byte
          NumLigComponents: int
          LastLigComponent: int
          LastClass: uint32
          XOffset: int
          YOffset: int
          MarkIndex: int }

    /// <summary>The fallback-mark state machine.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private MarkState =
        | MarkScan of index: int
        | MarkAround of around: AroundState
        | MarkZero of finish: int * markIndex: int
        | MarkDone

    /// <summary>The fixed inputs of the fallback-mark pass.</summary>
    [<Struct>]
    type private MarkPass =
        val Font: Font
        val BufferDirection: Direction
        val PlanDirection: Direction
        val Script: Script
        val Adjust: bool

        /// <summary>Creates a fallback-mark pass.</summary>
        /// <param name="font">The font.</param>
        /// <param name="bufferDirection">The buffer's direction after rotation.</param>
        /// <param name="planDirection">The plan's direction, the original one.</param>
        /// <param name="script">The script.</param>
        /// <param name="adjust">Whether offsets are adjusted when zeroing advances.</param>
        new (font, bufferDirection, planDirection, script, adjust) =
            { Font = font
              BufferDirection = bufferDirection
              PlanDirection = planDirection
              Script = script
              Adjust = adjust }

    /// <summary>Starts the positioning of the marks around a base whose extents answered, <c>position_around_base</c> after its extents
    /// are read: a run with no mark of a non-zero combining class changes nothing, any other starts the state of the loop.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="baseIndex">The base index.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <param name="baseExtents">The extents of the base glyph.</param>
    /// <returns>The edit and the next state.</returns>
    let private aroundBase (pass: MarkPass) (cursor: RunCursor) (baseIndex: int) (finish: int) (baseExtents: GlyphExtents) =
        match hasMarkInRange (cursor.InputProps) (baseIndex + 1) finish with
        | false -> struct (RunEdit.UnsafeToBreak (baseIndex, finish), MarkScan finish)
        | true ->
            let position = cursor.Positions[baseIndex]
            let baseExtents =
                { baseExtents with
                    YBearing = saturateAdd baseExtents.YBearing position.YOffset
                    XBearing = 0
                    Width = Font.hAdvance pass.Font cursor.Input[baseIndex].Codepoint }
            let props = cursor.InputProps[baseIndex]
            let ligId = GlyphPropsBits.ligId props
            let numLigComponents = int (GlyphPropsBits.ligNumComps props)
            let forward = not (Direction.isBackward pass.BufferDirection)
            let struct (xOffset, yOffset) =
                match forward with
                | true -> struct (saturateNeg position.XAdvance, saturateNeg position.YAdvance)
                | false -> struct (0, 0)
            struct (RunEdit.UnsafeToBreak (baseIndex, finish), MarkAround { Base = baseIndex; Finish = finish; BaseExtents = baseExtents; ComponentExtents = baseExtents; ClusterExtents = baseExtents; LigId = ligId; NumLigComponents = numLigComponents; LastLigComponent = -1; LastClass = 255u; XOffset = xOffset; YOffset = yOffset; MarkIndex = baseIndex + 1 })

    /// <summary>Starts a base: flags the cluster, and asks for its extents whatever the marks that follow, as <c>position_around_base</c>
    /// does: a base whose extents are unavailable has every non-spacing mark of its run zeroed, those of a combining class of zero
    /// included.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="baseIndex">The base index.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The edit and the next state.</returns>
    let private beginBase (pass: MarkPass) (cursor: RunCursor) (baseIndex: int) (finish: int) : struct (RunEdit * MarkState) =
        match Extents.glyph pass.Font cursor.Input[baseIndex].Codepoint with
        | Ok (ValueSome baseExtents) -> aroundBase pass cursor baseIndex finish baseExtents
        | Ok ValueNone -> struct (RunEdit.UnsafeToBreak (baseIndex, finish), MarkZero (finish, baseIndex + 1))
        | Error _ -> struct (RunEdit.UnsafeToBreak (baseIndex, finish), MarkScan finish)

    /// <summary>Accumulates a non-mark glyph's advances into the offsets of <c>position_around_base</c>.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The around state.</param>
    /// <returns>The edit and the next state.</returns>
    let private advanceStep (pass: MarkPass) (cursor: RunCursor) (state: AroundState) : struct (RunEdit * MarkState) =
        let position = cursor.Positions[state.MarkIndex]
        let forward = not (Direction.isBackward pass.BufferDirection)
        let xOffset, yOffset =
            match forward with
            | true -> saturateSub state.XOffset position.XAdvance, saturateSub state.YOffset position.YAdvance
            | false -> saturateAdd state.XOffset position.XAdvance, saturateAdd state.YOffset position.YAdvance
        struct (RunEdit.NextGlyphs 0, MarkAround { state with XOffset = xOffset; YOffset = yOffset; MarkIndex = state.MarkIndex + 1 })

    /// <summary>Positions one mark with a non-zero combining class.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The around state.</param>
    /// <returns>The edit and the next state.</returns>
    let private markStep (pass: MarkPass) (cursor: RunCursor) (state: AroundState) : struct (RunEdit * MarkState) =
        let props = cursor.InputProps[state.MarkIndex]
        let thisLigId = GlyphPropsBits.ligId props
        let thisLigComponent =
            let raw = int (GlyphPropsBits.ligComp props) - 1
            match state.LigId = 0uy || state.LigId <> thisLigId || raw >= state.NumLigComponents with
            | true -> state.NumLigComponents - 1
            | false -> raw
        let componentChanged = state.NumLigComponents > 1 && state.LastLigComponent <> thisLigComponent
        let componentExtents =
            match componentChanged with
            | false -> state.ComponentExtents
            | true ->
                let horizDir =
                    match Direction.isHorizontal pass.PlanDirection with
                    | true -> pass.PlanDirection
                    | false -> Script.horizontalDirection pass.Script
                let delta =
                    match horizDir with
                    | Direction.Ltr -> int64 thisLigComponent * int64 state.BaseExtents.Width / int64 state.NumLigComponents
                    | Direction.Rtl | Direction.Ttb | Direction.Btt | Direction.Invalid -> int64 (state.NumLigComponents - 1 - thisLigComponent) * int64 state.BaseExtents.Width / int64 state.NumLigComponents
                { state.BaseExtents with XBearing = clampToInt32 (int64 state.BaseExtents.XBearing + delta); Width = state.BaseExtents.Width / state.NumLigComponents }
        let lastLigComponent = if componentChanged then thisLigComponent else state.LastLigComponent
        let previousClass = if componentChanged then 255u else state.LastClass
        let thisClass = uint32 (UnicodeProps.modifiedCombiningClass props)
        let clusterExtents = if previousClass <> thisClass then componentExtents else state.ClusterExtents
        let next = { state with ComponentExtents = componentExtents; ClusterExtents = clusterExtents; LastLigComponent = lastLigComponent; LastClass = thisClass; MarkIndex = state.MarkIndex + 1 }
        match Extents.glyph pass.Font cursor.Input[state.MarkIndex].Codepoint with
        | Ok (ValueSome markExtents) ->
            let struct (clusterExtents, markXOffset, markYOffset) = positionMark pass.BufferDirection (pass.Font.YScale / 16) thisClass clusterExtents markExtents
            let position = cursor.Positions[state.MarkIndex]
            let positioned = { position with XAdvance = 0; YAdvance = 0; XOffset = saturateAdd markXOffset state.XOffset; YOffset = saturateAdd markYOffset state.YOffset }
            struct (RunEdit.SetPosition (state.MarkIndex, positioned), MarkAround { next with ClusterExtents = clusterExtents })
        | Ok ValueNone | Error _ ->
            let position = cursor.Positions[state.MarkIndex]
            let positioned = { position with XAdvance = 0; YAdvance = 0; XOffset = saturateAdd position.XOffset state.XOffset; YOffset = saturateAdd position.YOffset state.YOffset }
            struct (RunEdit.SetPosition (state.MarkIndex, positioned), MarkAround next)

    /// <summary>Advances one glyph of the mark run, positioning a mark or accumulating a non-mark advance.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="around">The around state.</param>
    /// <returns>The edit and the next state.</returns>
    let private aroundStep (pass: MarkPass) (cursor: RunCursor) (around: AroundState) : struct (RunEdit * MarkState) =
        match around.MarkIndex >= around.Finish with
        | true -> struct (RunEdit.NextGlyphs 0, MarkScan around.Finish)
        | false ->
            match UnicodeProps.modifiedCombiningClass cursor.InputProps[around.MarkIndex] <> 0uy with
            | false -> advanceStep pass cursor around
            | true -> markStep pass cursor around

    /// <summary>Zeroes the advances of the marks of a base whose extents are unavailable, <c>zero_mark_advances</c>.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <param name="markIndex">The next glyph index.</param>
    /// <returns>The edit and the next state.</returns>
    let private zeroStep (pass: MarkPass) (cursor: RunCursor) (finish: int) (markIndex: int) : struct (RunEdit * MarkState) =
        match markIndex >= finish with
        | true -> struct (RunEdit.NextGlyphs 0, MarkScan finish)
        | false ->
            match UnicodeProps.generalCategory cursor.InputProps[markIndex] = GeneralCategory.NonSpacingMark with
            | false -> struct (RunEdit.NextGlyphs 0, MarkZero (finish, markIndex + 1))
            | true ->
                let position = cursor.Positions[markIndex]
                let position =
                    match pass.Adjust with
                    | true -> { position with XOffset = saturateSub position.XOffset position.XAdvance; YOffset = saturateSub position.YOffset position.YAdvance }
                    | false -> position
                struct (RunEdit.SetPosition (markIndex, { position with XAdvance = 0; YAdvance = 0 }), MarkZero (finish, markIndex + 1))

    /// <summary>Scans for the next base and starts its mark run, <c>position_cluster_impl</c>'s outer loop.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="index">The next glyph index.</param>
    /// <returns>The edit and the next state.</returns>
    let private scanStep (pass: MarkPass) (cursor: RunCursor) (index: int) : struct (RunEdit * MarkState) =
        match index >= cursor.InputProps.Length with
        | true -> struct (RunEdit.Finish, MarkDone)
        | false ->
            match UnicodeProps.isUnicodeMark cursor.InputProps[index] with
            | true -> struct (RunEdit.NextGlyphs 0, MarkScan (index + 1))
            | false ->
                let finish = markRunEnd cursor.InputProps (index + 1)
                match finish > index + 1 with
                | false -> struct (RunEdit.NextGlyphs 0, MarkScan (index + 1))
                | true -> beginBase pass cursor index finish

    /// <summary>One step of the fallback-mark pass.</summary>
    /// <param name="pass">The pass.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The state.</param>
    /// <returns>The edit and the next state.</returns>
    let private step (pass: MarkPass) (cursor: RunCursor) (state: MarkState) : struct (RunEdit * MarkState) =
        match state with
        | MarkDone -> struct (RunEdit.Finish, MarkDone)
        | MarkScan index -> scanStep pass cursor index
        | MarkAround around -> aroundStep pass cursor around
        | MarkZero (finish, markIndex) -> zeroStep pass cursor finish markIndex

    /// <summary>The fallback-mark positioning pass.</summary>
    [<Struct>]
    type private MarkPositionPass =
        val Pass: MarkPass

        /// <summary>Creates a fallback-mark positioning pass.</summary>
        /// <param name="pass">The fixed inputs.</param>
        new (pass) = { Pass = pass }

        interface IRunPass<MarkState> with
            member this.Step (cursor, state) = step this.Pass cursor state

    /// <summary>Runs the positioning pass over a run whose face answers glyf extents.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjustOffsets">Whether offsets are adjusted when zeroing advances.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let private positionPass (plan: OtShapePlan) (font: Font) (adjustOffsets: bool) (run: GlyphRun) : GlyphRun =
        let pass = MarkPositionPass(MarkPass(font, run.SegmentProperties.Direction, plan.Props.Direction, plan.Props.Script, adjustOffsets))
        GlyphRun.runPass pass (MarkScan 0) run

    /// <summary>Positions the fallback marks of a run, or reports the face's unported outline source.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjustOffsets">Whether offsets are adjusted when zeroing advances.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, or the unported outline source when the run has a mark to position.</returns>
    let private positionMarks (plan: OtShapePlan) (font: Font) (adjustOffsets: bool) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match hasMarkToPosition (run.Props.AsSpan()) 0 with
        | false -> Ok (positionPass plan font adjustOffsets run)
        | true ->
            match unportedSource font with
            | ValueSome source -> Error (OutlinesNotPorted source)
            | ValueNone -> Ok (positionPass plan font adjustOffsets run)

    /// <summary>Positions the fallback marks of a run, <c>_hb_ot_shape_fallback_mark_position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjustOffsets">Whether offsets are adjusted when zeroing advances.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, the input run itself when no mark is positioned, or an unported outline source.</returns>
    let position (plan: OtShapePlan) (font: Font) (adjustOffsets: bool) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match run.Infos.Length < 2 || not run.HavePositions with
        | true -> Ok run
        | false ->
            match hasMarkCluster (run.Props.AsSpan()) 0 with
            | false -> Ok run
            | true -> positionMarks plan font adjustOffsets run
