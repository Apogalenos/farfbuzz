namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Unicode functions a shaping run asks, <c>hb_unicode_funcs_t</c>: the five that the shapers read, which a caller may
/// replace, the default set being <c>hb-ucd.cc</c>.</summary>
type internal UnicodeFuncs =
    { GeneralCategory: uint32 -> GeneralCategory
      CombiningClass: uint32 -> byte
      Mirroring: uint32 -> uint32
      Script: uint32 -> Script
      Compose: uint32 -> uint32 -> uint32 voption
      Decompose: uint32 -> struct (uint32 * uint32) voption }

/// <summary>The Unicode property functions of <c>hb-ucd.cc</c>.</summary>
[<RequireQualifiedAccess>]
module internal Unicode =
    /// <summary>The general-category values in upstream's coding order.</summary>
    let private categories : ImmutableArray<GeneralCategory> =
        Sealed.ofSpan (ReadOnlySpan [|
            Control; Format; Unassigned; PrivateUse; Surrogate
            LowercaseLetter; ModifierLetter; OtherLetter; TitlecaseLetter; UppercaseLetter
            SpacingMark; EnclosingMark; NonSpacingMark
            DecimalNumber; LetterNumber; OtherNumber
            ConnectPunctuation; DashPunctuation; ClosePunctuation; FinalPunctuation
            InitialPunctuation; OtherPunctuation; OpenPunctuation
            CurrencySymbol; ModifierSymbol; MathSymbol; OtherSymbol
            LineSeparator; ParagraphSeparator; SpaceSeparator |])

    /// <summary>Returns the general category, <c>hb_ucd_gc</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The category.</returns>
    let generalCategory (u) : GeneralCategory =
        categories[int (UcdTables.gc u)]

    /// <summary>Returns the canonical combining class, <c>hb_ucd_ccc</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The class.</returns>
    let combiningClass (u) =
        UcdTables.ccc u

    /// <summary>Returns the mirrored code point, <c>hb_ucd_mirroring</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The mirrored code point, or the input when it has none.</returns>
    let mirroring (u) =
        u + uint32 (UcdTables.bmg u)

    /// <summary>Returns the script, <c>hb_ucd_script</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The script.</returns>
    let script (u) : Script =
        UcdTables.scripts[int (UcdTables.sc u)]

    /// <summary>Returns the east Asian width, always 1 in the default funcs.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>1.</returns>
    let eastAsianWidth (u: uint32) =
        ignore u
        1u

    /// <summary>Encodes an 11/7/14 composition key, <c>HB_CODEPOINT_ENCODE3_11_7_14</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <param name="c">The composite.</param>
    /// <returns>The packed key.</returns>
    let private encode32 (a) (b) (c) =
        ((a &&& 0x7FFu) <<< 21) ||| ((b &&& 0x7Fu) <<< 14) ||| (c &&& 0x3FFFu)

    /// <summary>Encodes a 64-bit composition key, <c>HB_CODEPOINT_ENCODE3</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <param name="c">The composite.</param>
    /// <returns>The packed key.</returns>
    let private encode64 (a) (b) (c) =
        (uint64 a <<< 42) ||| (uint64 b <<< 21) ||| uint64 c

    /// <summary>Binary-searches a sorted uint32 array.</summary>
    /// <param name="values">The values.</param>
    /// <param name="key">The key.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The matching value, or none.</returns>
    [<TailCall>]
    let rec private search32 (values: ImmutableArray<uint32>) (key) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = (lo + hi) / 2
            let value = values[mid] &&& encode32 0x1FFFFFu 0x1FFFFFu 0u
            match value with
            | value when value < key -> search32 values key (mid + 1) hi
            | value when value > key -> search32 values key lo (mid - 1)
            | _ -> ValueSome values[mid]

    /// <summary>Binary-searches a sorted uint64 array.</summary>
    /// <param name="values">The values.</param>
    /// <param name="key">The key.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The matching value, or none.</returns>
    [<TailCall>]
    let rec private search64 (values: ImmutableArray<uint64>) (key) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = (lo + hi) / 2
            let value = values[mid] &&& encode64 0x1FFFFFu 0x1FFFFFu 0u
            match value with
            | value when value < key -> search64 values key (mid + 1) hi
            | value when value > key -> search64 values key lo (mid - 1)
            | _ -> ValueSome values[mid]

    /// <summary>The Hangul syllable base, <c>SBASE</c>.</summary>
    [<Literal>]
    let private SBase = 0xAC00u

    /// <summary>The Hangul leading consonant base, <c>LBASE</c>.</summary>
    [<Literal>]
    let private LBase = 0x1100u

    /// <summary>The Hangul vowel base, <c>VBASE</c>.</summary>
    [<Literal>]
    let private VBase = 0x1161u

    /// <summary>The Hangul trailing consonant base, <c>TBASE</c>.</summary>
    [<Literal>]
    let private TBase = 0x11A7u

    /// <summary>The number of Hangul syllables, <c>SCOUNT</c>.</summary>
    [<Literal>]
    let private SCount = 11172u

    /// <summary>The number of Hangul leading consonants, <c>LCOUNT</c>.</summary>
    [<Literal>]
    let private LCount = 19u

    /// <summary>The number of Hangul vowels, <c>VCOUNT</c>.</summary>
    [<Literal>]
    let private VCount = 21u

    /// <summary>The number of Hangul trailing consonants, <c>TCOUNT</c>.</summary>
    [<Literal>]
    let private TCount = 28u

    /// <summary>The number of LV syllables, <c>NCOUNT</c>.</summary>
    [<Literal>]
    let private NCount = 588u

    /// <summary>Composes the Hangul algorithm, <c>_hb_ucd_compose_hangul</c>.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The syllable, or none.</returns>
    let private composeHangul (a) (b) =
        match a >= SBase && a < SBase + SCount && b > TBase && b < TBase + TCount && (a - SBase) % TCount = 0u,
              a >= LBase && a < LBase + LCount && b >= VBase && b < VBase + VCount with
        | true, _ -> ValueSome (a + (b - TBase))
        | false, true -> ValueSome (SBase + (a - LBase) * NCount + (b - VBase) * TCount)
        | false, false -> ValueNone

    /// <summary>Composes through the 32-bit table.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let private compose32 (a) (b) =
        match search32 UcdTables.dm2U32 (encode32 a b 0u) 0 (UcdTables.dm2U32.Length - 1) with
        | ValueSome value ->
            let composite = value &&& 0x3FFFu
            match composite with | 0u -> ValueNone | _ -> ValueSome composite
        | ValueNone -> ValueNone

    /// <summary>Composes through the 64-bit table.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let private compose64 (a) (b) =
        match search64 UcdTables.dm2U64 (encode64 a b 0u) 0 (UcdTables.dm2U64.Length - 1) with
        | ValueSome value ->
            let composite = uint32 (value &&& 0x1FFFFFUL)
            match composite with | 0u -> ValueNone | _ -> ValueSome composite
        | ValueNone -> ValueNone

    /// <summary>Composes two code points by the binary searches of upstream's tables, <c>hb_ucd_compose</c> as it is written: the
    /// reference <see cref="compose"/> is equal to.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let composeBySearch (a) (b) =
        match a = 0u || b = 0u with
        | true -> ValueNone
        | false ->
            match composeHangul a b with
            | ValueSome _ as hangul -> hangul
            | ValueNone ->
                match a &&& 0xFFFFF800u = 0u && b &&& 0xFFFFFF80u = 0x300u with
                | true -> compose32 a b
                | false -> compose64 a b

    /// <summary>The key of a pair in a composition table, the first code point above the second.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The key, never zero for a first code point that is not zero.</returns>
    let private pairKey (a: uint32) (b: uint32) =
        (uint64 a <<< 32) ||| uint64 b

    /// <summary>The compositions of the 32-bit table as a hash table: the pairs and their composites, read from the table's entries.</summary>
    let struct (private smallKeys, private smallValues, private smallShift) =
        Sealed.keyTable
            (Sealed.init UcdTables.dm2U32.Length (fun index ->
                let value = UcdTables.dm2U32[index]
                pairKey (value >>> 21) (((value >>> 14) &&& 0x7Fu) ||| 0x300u)))
            (Sealed.init UcdTables.dm2U32.Length (fun index -> UcdTables.dm2U32[index] &&& 0x3FFFu))

    /// <summary>The compositions of the 64-bit table as a hash table: the pairs and their composites, read from the table's entries.</summary>
    let struct (private wideKeys, private wideValues, private wideShift) =
        Sealed.keyTable
            (Sealed.init UcdTables.dm2U64.Length (fun index ->
                let value = UcdTables.dm2U64[index]
                pairKey (uint32 (value >>> 42)) (uint32 ((value >>> 21) &&& 0x1FFFFFUL))))
            (Sealed.init UcdTables.dm2U64.Length (fun index -> uint32 (UcdTables.dm2U64[index] &&& 0x1FFFFFUL)))

    /// <summary>The code points below U+10000 that are the second of some composition pair, one bit each: a second code point that is
    /// not in it composes with nothing, whatever the first.</summary>
    let private secondBits : ImmutableArray<uint64> = Sealed.lowSecondBits smallKeys wideKeys

    /// <summary>Probes a composition hash table for a key.</summary>
    /// <param name="keys">The slots' keys.</param>
    /// <param name="values">The slots' values.</param>
    /// <param name="shift">The shift of the slot.</param>
    /// <param name="key">The key.</param>
    /// <param name="slot">The slot to look at.</param>
    /// <returns>The composite, or none for a key that is absent or whose composite is zero.</returns>
    [<TailCall>]
    let rec private probe (keys: ImmutableArray<uint64>) (values: ImmutableArray<uint32>) shift key slot =
        match keys[slot] with
        | 0UL -> ValueNone
        | found when found = key ->
            match values[slot] with
            | 0u -> ValueNone
            | composite -> ValueSome composite
        | _ -> probe keys values shift key ((slot + 1) &&& (keys.Length - 1))

    /// <summary>Composes through a composition hash table.</summary>
    /// <param name="keys">The slots' keys.</param>
    /// <param name="values">The slots' values.</param>
    /// <param name="shift">The shift of the slot.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let private composeHashed (keys: ImmutableArray<uint64>) (values: ImmutableArray<uint32>) shift a b =
        let key = pairKey a b
        probe keys values shift key (Sealed.keyTableSlot key shift)

    /// <summary>Composes two code points, <c>hb_ucd_compose</c>: the pairs of upstream's tables are found through a hash table built
    /// once from them, not by the binary searches of <see cref="composeBySearch"/>, whose result it equals.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let compose (a) (b) =
        match a = 0u || b = 0u with
        | true -> ValueNone
        | false ->
            match composeHangul a b with
            | ValueSome _ as hangul -> hangul
            | ValueNone ->
                match b < 0x10000u && secondBits[int (b >>> 6)] &&& (1UL <<< int (b &&& 63u)) = 0UL, a &&& 0xFFFFF800u = 0u && b &&& 0xFFFFFF80u = 0x300u with
                | true, _ -> ValueNone
                | false, true -> composeHashed smallKeys smallValues smallShift a b
                | false, false -> composeHashed wideKeys wideValues wideShift a b

    /// <summary>Whether a code point may be the second of a pair <see cref="compose"/> composes: the Hangul vowels and trailing
    /// consonants, the second code points of the tables, and every code point from U+10000, which the filter does not cover. A code
    /// point for which this is false composes with nothing, whatever the first.</summary>
    /// <param name="b">The second code point.</param>
    /// <returns>False only for a code point that composes with no first code point.</returns>
    let mightBeSecond (b: uint32) =
        match b < 0x10000u with
        | false -> true
        | true -> secondBits[int (b >>> 6)] &&& (1UL <<< int (b &&& 63u)) <> 0UL || (b >= VBase && b < VBase + VCount) || (b > TBase && b < TBase + TCount)

    /// <summary>Decomposes the Hangul algorithm, <c>_hb_ucd_decompose_hangul</c>.</summary>
    /// <param name="ab">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let private decomposeHangul (ab) =
        let si = ab - SBase
        match si < SCount, si % TCount with
        | true, 0u -> ValueSome (struct (LBase + si / NCount, VBase + (si % NCount) / TCount))
        | true, _ -> ValueSome (struct (SBase + (si / TCount) * TCount, TBase + (si % TCount)))
        | false, _ -> ValueNone

    /// <summary>Decomposes a single-element table entry.</summary>
    /// <param name="index">The one-based decomposition index.</param>
    /// <returns>The two parts, or none.</returns>
    let private decomposeSingle (index) =
        let p0 = UcdTables.dm1P0.Length
        match index < p0 with
        | true -> ValueSome (struct (uint32 UcdTables.dm1P0[index], 0u))
        | false -> ValueSome (struct (0x20000u ||| uint32 UcdTables.dm1P2[index - p0], 0u))

    /// <summary>Decomposes a two-element table entry.</summary>
    /// <param name="index">The index into the two-element arrays.</param>
    /// <returns>The two parts.</returns>
    let private decomposePair (index) =
        match index < UcdTables.dm2U32.Length with
        | true ->
            let value = UcdTables.dm2U32[index]
            ValueSome (struct (value >>> 21, ((value >>> 14) &&& 0x7Fu) ||| 0x300u))
        | false ->
            let value = UcdTables.dm2U64[index - UcdTables.dm2U32.Length]
            ValueSome (struct (uint32 (value >>> 42), uint32 ((value >>> 21) &&& 0x1FFFFFUL)))

    /// <summary>Decomposes through the generated table.</summary>
    /// <param name="ab">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let private decomposeTable (ab) =
        let i = int (UcdTables.dm ab)
        match i with
        | 0 -> ValueNone
        | _ ->
            let index = i - 1
            let p0 = UcdTables.dm1P0.Length
            let p2 = UcdTables.dm1P2.Length
            match index < p0 + p2 with
            | true -> decomposeSingle index
            | false -> decomposePair (index - p0 - p2)

    /// <summary>Decomposes one code point, <c>hb_ucd_decompose</c>.</summary>
    /// <param name="ab">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decompose (ab) : struct (uint32 * uint32) voption =
        match ab >= SBase && ab < SBase + SCount with
        | true -> decomposeHangul ab
        | false -> decomposeTable ab

    /// <summary>Returns no compatibility decomposition; the default func does not implement one.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>An empty array.</returns>
    let decomposeCompatibility (u: uint32) : ImmutableArray<uint32> =
        ignore u
        ImmutableArray.Empty

    /// <summary>The modified combining class of a code point whose combining class is known, <c>hb_unicode_modified_combining_class</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <param name="klass">The combining class the functions answer for it.</param>
    /// <returns>The reordered class.</returns>
    let modifiedClassOf (u) (klass: byte) =
        match u with
        | 0x1A60u | 0x0FC6u -> 254uy
        | 0x0F39u -> 127uy
        | _ ->
            match klass with
            | 10uy -> 22uy | 11uy -> 15uy | 12uy -> 16uy | 13uy -> 17uy | 14uy -> 23uy
            | 15uy -> 18uy | 16uy -> 19uy | 17uy -> 20uy | 18uy -> 21uy | 19uy -> 14uy
            | 20uy -> 24uy | 21uy -> 12uy | 22uy -> 25uy | 23uy -> 13uy | 24uy -> 10uy | 25uy -> 11uy
            | 27uy -> 28uy | 28uy -> 29uy | 29uy -> 30uy | 30uy -> 31uy | 31uy -> 32uy | 32uy -> 33uy | 33uy -> 27uy
            | 84uy -> 4uy | 91uy -> 5uy | 103uy -> 3uy | 130uy -> 132uy | 132uy -> 131uy
            | value -> value

    /// <summary>The modified combining class, <c>hb_unicode_modified_combining_class</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The reordered class.</returns>
    let modifiedCombiningClass (u) =
        modifiedClassOf u (combiningClass u)

    /// <summary>The default functions, the set <c>hb_unicode_funcs_get_default</c> answers.</summary>
    let defaults =
        { GeneralCategory = generalCategory
          CombiningClass = combiningClass
          Mirroring = mirroring
          Script = script
          Compose = compose
          Decompose = decompose }

    /// <summary>The modified combining class through the given functions, <c>hb_unicode_funcs_t::modified_combining_class</c>.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The reordered class.</returns>
    let modifiedBy (funcs: UnicodeFuncs) (u) =
        modifiedClassOf u (funcs.CombiningClass u)

    /// <summary>Whether a code point is a variation selector, <c>is_variation_selector</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+FE00..FE0F and U+E0100..E01EF.</returns>
    let isVariationSelector (u) =
        (u >= 0xFE00u && u <= 0xFE0Fu) || (u >= 0xE0100u && u <= 0xE01EFu)

    /// <summary>Whether a code point is default-ignorable, <c>is_default_ignorable</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True when HarfBuzz hides it by default.</returns>
    let isDefaultIgnorable (u) =
        let plane = u >>> 16
        match plane with
        | 0u ->
            match u >>> 8 with
            | 0x00u -> u = 0x00ADu
            | 0x03u -> u = 0x034Fu
            | 0x06u -> u = 0x061Cu
            | 0x17u -> u >= 0x17B4u && u <= 0x17B5u
            | 0x18u -> u >= 0x180Bu && u <= 0x180Fu
            | 0x20u -> (u >= 0x200Bu && u <= 0x200Fu) || (u >= 0x202Au && u <= 0x202Eu) || (u >= 0x2060u && u <= 0x206Fu)
            | 0xFEu -> (u >= 0xFE00u && u <= 0xFE0Fu) || u = 0xFEFFu
            | 0xFFu -> u >= 0xFFF0u && u <= 0xFFF8u
            | _ -> false
        | 0x01u -> u >= 0x1D173u && u <= 0x1D17Au
        | 0x0Eu -> u >= 0xE0000u && u <= 0xE0FFFu
        | _ -> false

    /// <summary>Returns the fallback space width, <c>space_fallback_type</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The space estimate.</returns>
    let spaceFallbackType (u) : SpaceFallback =
        match u with
        | 0x0020u | 0x00A0u -> Space
        | 0x2000u | 0x2002u -> SpaceEm2
        | 0x2001u | 0x2003u | 0x3000u -> SpaceEm
        | 0x2004u -> SpaceEm3
        | 0x2005u -> SpaceEm4
        | 0x2006u -> SpaceEm6
        | 0x2007u -> SpaceFigure
        | 0x2008u -> SpacePunctuation
        | 0x2009u -> SpaceEm5
        | 0x200Au -> SpaceEm16
        | 0x202Fu -> SpaceNarrow
        | 0x205Fu -> Space4Em18
        | _ -> NotSpace

    /// <summary>Returns the vertical form of a code point, <c>vertical_char_for</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The vertical form, or the input.</returns>
    let verticalCharFor (u) =
        match u with
        | 0x2013u -> 0xFE32u | 0x2014u -> 0xFE31u | 0x2025u -> 0xFE30u | 0x2026u -> 0xFE19u
        | 0x3001u -> 0xFE11u | 0x3002u -> 0xFE12u | 0x3008u -> 0xFE3Fu | 0x3009u -> 0xFE40u
        | 0x300Au -> 0xFE3Du | 0x300Bu -> 0xFE3Eu | 0x300Cu -> 0xFE41u | 0x300Du -> 0xFE42u
        | 0x300Eu -> 0xFE43u | 0x300Fu -> 0xFE44u | 0x3010u -> 0xFE3Bu | 0x3011u -> 0xFE3Cu
        | 0x3014u -> 0xFE39u | 0x3015u -> 0xFE3Au | 0x3016u -> 0xFE17u | 0x3017u -> 0xFE18u
        | 0xFE4Fu -> 0xFE34u
        | 0xFF01u -> 0xFE15u | 0xFF08u -> 0xFE35u | 0xFF09u -> 0xFE36u | 0xFF0Cu -> 0xFE10u
        | 0xFF1Au -> 0xFE13u | 0xFF1Bu -> 0xFE14u | 0xFF1Fu -> 0xFE16u | 0xFF3Bu -> 0xFE47u
        | 0xFF3Du -> 0xFE48u | 0xFF3Fu -> 0xFE33u | 0xFF5Bu -> 0xFE37u | 0xFF5Du -> 0xFE38u
        | _ -> u

    /// <summary>Whether a code point is Extended_Pictographic.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The generated property.</returns>
    let isExtendedPictographic (u) =
        EmojiTables.isExtendedPictographic u
