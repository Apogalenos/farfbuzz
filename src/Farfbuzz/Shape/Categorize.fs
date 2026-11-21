namespace Farfbuzz

/// <summary>The shaper choice and the fixed profiles of <c>hb-ot-shaper.hh</c> and the phase-4 shaper stubs.</summary>
[<RequireQualifiedAccess>]
module internal Categorize =
    /// <summary>The pseudo-script tag <c>HB_SCRIPT_MYANMAR_ZAWGYI</c>, spelled 'Qaag'.</summary>
    [<Literal>]
    let private MyanmarZawgyiScript = 0x51616167u<script>

    /// <summary>The Hebrew shaper's GPOS script tag, 'hebr' (hb-ot-shaper-hebrew.cc:204).</summary>
    [<Literal>]
    let private HebrewGposTag = 0x68656272u<tag>

    /// <summary>The low byte of the USE script tags of Indic scripts, ASCII '3'.</summary>
    [<Literal>]
    let private UseTagByte = 0x33u

    /// <summary>Classifies a script, direction and chosen GSUB script into a shaper, <c>hb_ot_shaper_categorize</c>.</summary>
    /// <param name="script">The script.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="gsubScript">The chosen GSUB script tag.</param>
    /// <returns>The shaper kind.</returns>
    let shaper (script: Script) (direction: Direction) (gsubScript: Tag) : ShaperKind =
        let horizontal = Direction.isHorizontal direction
        match script with
        | Scripts.Arabic | Scripts.Syriac ->
            match (gsubScript <> Tags.Dflt || script = Scripts.Arabic) && horizontal with
            | true -> ArabicShaper
            | false -> DefaultShaper
        | Scripts.Thai | Scripts.Lao -> ThaiShaper
        | Scripts.Hangul -> HangulShaper
        | Scripts.Hebrew -> HebrewShaper
        | Scripts.Bengali | Scripts.Devanagari | Scripts.Gujarati | Scripts.Gurmukhi | Scripts.Kannada
        | Scripts.Malayalam | Scripts.Oriya | Scripts.Tamil | Scripts.Telugu ->
            match gsubScript = Tags.Dflt || gsubScript = Tags.Latn with
            | true -> DefaultShaper
            | false ->
                match (uint32 gsubScript &&& 0xFFu) = UseTagByte with
                | true -> UseShaper
                | false -> IndicShaper
        | Scripts.Khmer -> KhmerShaper
        | Scripts.Myanmar ->
            match gsubScript = Tags.Dflt || gsubScript = Tags.Latn || gsubScript = Tags.Mymr with
            | true -> DefaultShaper
            | false -> MyanmarShaper
        | MyanmarZawgyiScript -> MyanmarZawgyiShaper
        | Scripts.Tibetan | Scripts.Mongolian | Scripts.Sinhala | Scripts.Buhid | Scripts.Hanunoo | Scripts.Tagalog
        | Scripts.Tagbanwa | Scripts.Limbu | Scripts.TaiLe | Scripts.Buginese | Scripts.Kharoshthi
        | Scripts.SylotiNagri | Scripts.Tifinagh | Scripts.Balinese | Scripts.Nko | Scripts.PhagsPa | Scripts.Cham
        | Scripts.KayahLi | Scripts.Lepcha | Scripts.Rejang | Scripts.Saurashtra | Scripts.Sundanese
        | Scripts.EgyptianHieroglyphs | Scripts.Javanese | Scripts.Kaithi | Scripts.MeeteiMayek | Scripts.TaiTham
        | Scripts.TaiViet | Scripts.Batak | Scripts.Brahmi | Scripts.Mandaic | Scripts.Chakma | Scripts.Miao
        | Scripts.Sharada | Scripts.Takri | Scripts.Duployan | Scripts.Grantha | Scripts.Khojki | Scripts.Khudawadi
        | Scripts.Mahajani | Scripts.Manichaean | Scripts.Modi | Scripts.PahawhHmong | Scripts.PsalterPahlavi
        | Scripts.Siddham | Scripts.Tirhuta | Scripts.Ahom | Scripts.Multani | Scripts.Adlam | Scripts.Bhaiksuki
        | Scripts.Marchen | Scripts.Newa | Scripts.MasaramGondi | Scripts.Soyombo | Scripts.ZanabazarSquare
        | Scripts.Dogra | Scripts.GunjalaGondi | Scripts.HanifiRohingya | Scripts.Makasar | Scripts.Medefaidrin
        | Scripts.OldSogdian | Scripts.Sogdian | Scripts.Elymaic | Scripts.Nandinagari | Scripts.NyiakengPuachueHmong
        | Scripts.Wancho | Scripts.Chorasmian | Scripts.DivesAkuru | Scripts.KhitanSmallScript | Scripts.Yezidi
        | Scripts.CyproMinoan | Scripts.OldUyghur | Scripts.Tangsa | Scripts.Toto | Scripts.Vithkuqi | Scripts.Kawi
        | Scripts.NagMundari | Scripts.Garay | Scripts.GurungKhema | Scripts.KiratRai | Scripts.OlOnal | Scripts.Sunuwar
        | Scripts.Todhri | Scripts.TuluTigalari | Scripts.BeriaErfe | Scripts.Sidetic | Scripts.TaiYo | Scripts.TolongSiki
        | Scripts.Jurchen | Scripts.ProtoCuneiform | Scripts.Seal ->
            match gsubScript = Tags.Dflt || gsubScript = Tags.Latn with
            | true -> DefaultShaper
            | false -> UseShaper
        | _ -> DefaultShaper

    /// <summary>Returns a shaper's zero-width-marks mode, the <c>zero_width_marks</c> field of its
    /// <c>hb_ot_shaper_t</c> (hb-ot-shaper-default.cc:48,69; hb-ot-shaper-arabic.cc:800; hb-ot-shaper-thai.cc:386;
    /// hb-ot-shaper-hangul.cc:447; hb-ot-shaper-hebrew.cc:206; hb-ot-shaper-indic.cc:1536; hb-ot-shaper-khmer.cc:376;
    /// hb-ot-shaper-myanmar.cc:361,384; hb-ot-shaper-use.cc:509).</summary>
    /// <param name="kind">The shaper kind.</param>
    /// <returns>The mode.</returns>
    let zeroWidthMarks (kind: ShaperKind) : ZeroWidthMarks =
        match kind with
        | DefaultShaper -> ZeroWidthMarksByGdefLate
        | DumberShaper -> ZeroWidthMarksNone
        | ArabicShaper -> ZeroWidthMarksByGdefLate
        | ThaiShaper -> ZeroWidthMarksByGdefLate
        | HangulShaper -> ZeroWidthMarksNone
        | HebrewShaper -> ZeroWidthMarksByGdefLate
        | IndicShaper -> ZeroWidthMarksNone
        | KhmerShaper -> ZeroWidthMarksNone
        | MyanmarShaper -> ZeroWidthMarksByGdefEarly
        | MyanmarZawgyiShaper -> ZeroWidthMarksNone
        | UseShaper -> ZeroWidthMarksByGdefEarly

    /// <summary>Returns whether a shaper positions marks by fallback, the <c>fallback_position</c> field of its
    /// <c>hb_ot_shaper_t</c> (hb-ot-shaper-default.cc:49,70; hb-ot-shaper-arabic.cc:801; hb-ot-shaper-thai.cc:387;
    /// hb-ot-shaper-hangul.cc:448; hb-ot-shaper-hebrew.cc:207; hb-ot-shaper-indic.cc:1537; hb-ot-shaper-khmer.cc:377;
    /// hb-ot-shaper-myanmar.cc:362,385; hb-ot-shaper-use.cc:510).</summary>
    /// <param name="kind">The shaper kind.</param>
    /// <returns>True when the shaper positions marks by fallback.</returns>
    let fallbackPosition (kind: ShaperKind) : bool =
        match kind with
        | DefaultShaper -> true
        | DumberShaper -> false
        | ArabicShaper -> true
        | ThaiShaper -> false
        | HangulShaper -> true
        | HebrewShaper -> true
        | IndicShaper -> false
        | KhmerShaper -> false
        | MyanmarShaper -> false
        | MyanmarZawgyiShaper -> false
        | UseShaper -> false

    /// <summary>Returns the fixed profile of a shaper, the tail data members of <c>hb_ot_shaper_t</c>, one complete
    /// profile per shaper kind.</summary>
    /// <param name="kind">The shaper kind.</param>
    /// <returns>The profile.</returns>
    let profile (kind: ShaperKind) : ShaperProfile =
        match kind with
        | DefaultShaper ->
            { Kind = DefaultShaper
              GposTag = Tags.Empty
              Normalization = NormalizationAuto
              ZeroWidthMarks = zeroWidthMarks DefaultShaper
              FallbackPosition = fallbackPosition DefaultShaper }
        | DumberShaper ->
            { Kind = DumberShaper
              GposTag = Tags.Empty
              Normalization = NormalizationAuto
              ZeroWidthMarks = zeroWidthMarks DumberShaper
              FallbackPosition = fallbackPosition DumberShaper }
        | ThaiShaper ->
            { Kind = ThaiShaper
              GposTag = Tags.Empty
              Normalization = NormalizationAuto
              ZeroWidthMarks = zeroWidthMarks ThaiShaper
              FallbackPosition = fallbackPosition ThaiShaper }
        | HebrewShaper ->
            { Kind = HebrewShaper
              GposTag = HebrewGposTag
              Normalization = NormalizationAuto
              ZeroWidthMarks = zeroWidthMarks HebrewShaper
              FallbackPosition = fallbackPosition HebrewShaper }
        | HangulShaper ->
            { Kind = HangulShaper
              GposTag = Tags.Empty
              Normalization = NormalizationNone
              ZeroWidthMarks = zeroWidthMarks HangulShaper
              FallbackPosition = fallbackPosition HangulShaper }
        | KhmerShaper ->
            { Kind = KhmerShaper
              GposTag = Tags.Empty
              Normalization = NormalizationComposedDiacriticsNoShortCircuit
              ZeroWidthMarks = zeroWidthMarks KhmerShaper
              FallbackPosition = fallbackPosition KhmerShaper }
        | IndicShaper ->
            { Kind = IndicShaper
              GposTag = Tags.Empty
              Normalization = NormalizationComposedDiacriticsNoShortCircuit
              ZeroWidthMarks = zeroWidthMarks IndicShaper
              FallbackPosition = fallbackPosition IndicShaper }
        | UseShaper ->
            { Kind = UseShaper
              GposTag = Tags.Empty
              Normalization = NormalizationComposedDiacriticsNoShortCircuit
              ZeroWidthMarks = zeroWidthMarks UseShaper
              FallbackPosition = fallbackPosition UseShaper }
        | MyanmarShaper ->
            { Kind = MyanmarShaper
              GposTag = Tags.Empty
              Normalization = NormalizationComposedDiacriticsNoShortCircuit
              ZeroWidthMarks = zeroWidthMarks MyanmarShaper
              FallbackPosition = fallbackPosition MyanmarShaper }
        | MyanmarZawgyiShaper ->
            { Kind = MyanmarZawgyiShaper
              GposTag = Tags.Empty
              Normalization = NormalizationNone
              ZeroWidthMarks = zeroWidthMarks MyanmarZawgyiShaper
              FallbackPosition = fallbackPosition MyanmarZawgyiShaper }
        | ArabicShaper ->
            { Kind = ArabicShaper
              GposTag = Tags.Empty
              Normalization = NormalizationAuto
              ZeroWidthMarks = zeroWidthMarks ArabicShaper
              FallbackPosition = fallbackPosition ArabicShaper }
