namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// <summary>The lookup flag bits, <c>LookupFlag</c> (src/hb-ot-layout-common.hh:1295-1300).</summary>
[<RequireQualifiedAccess>]
module internal LookupFlag =
    /// <summary>The right-to-left bit, <c>LookupFlag::RightToLeft</c>.</summary>
    [<Literal>]
    let RightToLeft = 0x0001u

    /// <summary>The ignore-base-glyphs bit, <c>LookupFlag::IgnoreBaseGlyphs</c>.</summary>
    [<Literal>]
    let IgnoreBaseGlyphs = 0x0002u

    /// <summary>The ignore-ligatures bit, <c>LookupFlag::IgnoreLigatures</c>.</summary>
    [<Literal>]
    let IgnoreLigatures = 0x0004u

    /// <summary>The ignore-marks bit, <c>LookupFlag::IgnoreMarks</c>.</summary>
    [<Literal>]
    let IgnoreMarks = 0x0008u

    /// <summary>The ignore flags, <c>LookupFlag::IgnoreFlags</c>.</summary>
    [<Literal>]
    let IgnoreFlags = 0x000Eu

    /// <summary>The use-mark-filtering-set bit, <c>LookupFlag::UseMarkFilteringSet</c>.</summary>
    [<Literal>]
    let UseMarkFilteringSet = 0x0010u

    /// <summary>The mark-attachment-type mask, <c>LookupFlag::MarkAttachmentType</c>.</summary>
    [<Literal>]
    let MarkAttachmentType = 0xFF00u

/// <summary>The matcher value, <c>matcher_t</c> (src/hb-ot-layout-gsubgpos.hh:834-916).</summary>
[<Struct>]
type internal Matcher =
    { LookupProps: uint32
      Mask: uint32
      IgnoreZwnj: bool
      IgnoreZwj: bool
      IgnoreHidden: bool
      PerSyllable: bool
      Syllable: byte }

/// <summary>The skip classification of a glyph, <c>matcher_t::may_skip_t</c>.</summary>
[<Struct>]
type internal SkipVerdict =
    | SkipYes
    | SkipNo
    | SkipMaybe

/// <summary>The match classification of a glyph, <c>skipping_iterator_t::match_t</c>.</summary>
[<Struct>]
type internal MatchVerdict =
    | Matched
    | NotMatched
    | Skipped

/// <summary>The result of one iterator step: the matched index, or the unsafe bound of a miss.</summary>
[<Struct>]
type internal Probe =
    | Hit of hitIndex: int
    | Miss of unsafeBound: int

/// <summary>Where the first step of a skipping walk stops, before any match function is asked: the walk reached the end of the
/// buffer, stopped at a glyph the lookup does not skip, or passed one whose skipping depends on the match function.</summary>
[<Struct>]
type internal WalkHead =
    | HeadEnd
    | HeadGlyph of glyphIndex: int
    | HeadMaybe

/// <summary>The active patterns of the matching core.</summary>
[<AutoOpen>]
module internal MatchingPatterns =
    /// <summary>The glyph's class is one the lookup ignores (src/hb-ot-layout-gsubgpos.hh:1329).</summary>
    /// <param name="lookupProps">The lookup props.</param>
    /// <param name="glyphProps">The glyph props.</param>
    /// <returns>True when an ignore flag matches.</returns>
    let (|IgnoredClass|_|) (lookupProps: uint32) (glyphProps: uint16) : bool =
        uint32 glyphProps &&& lookupProps &&& LookupFlag.IgnoreFlags <> 0u

    /// <summary>The lookup filters marks through a GDEF mark glyph set; yields the set index
    /// (src/hb-ot-layout-gsubgpos.hh:1302-1306).</summary>
    /// <param name="lookupProps">The lookup props.</param>
    /// <returns>The set index, or none.</returns>
    [<return: Struct>]
    let (|MarkFilteringSet|_|) (lookupProps: uint32) : int voption =
        match lookupProps &&& LookupFlag.UseMarkFilteringSet with
        | 0u -> ValueNone
        | _ -> ValueSome (int (lookupProps >>> 16))

    /// <summary>The lookup only accepts marks of one attachment class; yields the class bits
    /// (src/hb-ot-layout-gsubgpos.hh:1312-1313).</summary>
    /// <param name="lookupProps">The lookup props.</param>
    /// <returns>The attachment-class bits, or none.</returns>
    [<return: Struct>]
    let (|MarkAttachmentClass|_|) (lookupProps: uint32) : uint32 voption =
        match lookupProps &&& LookupFlag.MarkAttachmentType with
        | 0u -> ValueNone
        | bits -> ValueSome bits

    /// <summary>The glyph is a mark according to its glyph props (src/hb-ot-layout.hh:560-564).</summary>
    /// <param name="glyphProps">The glyph props.</param>
    /// <returns>True when the mark bit is set.</returns>
    let (|MarkGlyph|_|) (glyphProps: uint16) : bool =
        glyphProps &&& GlyphPropsBits.Mark <> 0us

    /// <summary>Yields the matched index of a probe (the voption face of skippy_iter.next/prev).</summary>
    /// <param name="probe">The probe.</param>
    /// <returns>The hit index, or none.</returns>
    [<return: Struct>]
    let (|Found|_|) (probe: Probe) : int voption =
        match probe with
        | Hit index -> ValueSome index
        | Miss _ -> ValueNone

/// <summary>The glyph data a rule matches against, specialised by the JIT per implementation.</summary>
type internal IGlyphData =
    /// <summary>Whether a match function is present.</summary>
    abstract HasMatchFunc : bool
    /// <summary>Whether the glyph at <paramref name="position"/> matches the glyph data.</summary>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="position">The glyph-data index.</param>
    /// <returns>True when the glyph matches.</returns>
    abstract Matches : info: GlyphInfo * props: GlyphProps * position: int -> bool

/// <summary>The matching core: <c>matcher_t</c>, the skipping iterator, glyph classes and the buffer serial.</summary>
[<RequireQualifiedAccess>]
module internal Matching =
    /// <summary>check_glyph_property (src/hb-ot-layout-gsubgpos.hh:1321-1335), pure.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="glyphProps">The glyph props.</param>
    /// <param name="lookupProps">The lookup props.</param>
    /// <returns>True when the glyph is not filtered out.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let checkGlyphProperty (gdef: Gdef) (glyph: uint32) (glyphProps: uint16) (lookupProps: uint32) : bool =
        match glyphProps with
        | IgnoredClass lookupProps -> false
        | MarkGlyph ->
            match lookupProps with
            | MarkFilteringSet set -> Gdef.markSetCovers gdef set glyph
            | MarkAttachmentClass bits -> bits = (uint32 glyphProps &&& LookupFlag.MarkAttachmentType)
            | _ -> true
        | _ -> true

    /// <summary>matcher_t::may_skip (src/hb-ot-layout-gsubgpos.hh:886-903).</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The skip verdict.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let maySkip (m: Matcher) (gdef: Gdef) (info: GlyphInfo) (props: GlyphProps) : SkipVerdict =
        match checkGlyphProperty gdef info.Codepoint props.GlyphClass m.LookupProps with
        | false -> SkipYes
        | true ->
            match UnicodeProps.isDefaultIgnorable props
                  && (m.IgnoreZwnj || not (UnicodeProps.isZwnj props))
                  && (m.IgnoreZwj || not (UnicodeProps.isZwj props))
                  && (m.IgnoreHidden || not (UnicodeProps.isHidden props)) with
            | true -> SkipMaybe
            | false -> SkipNo

    /// <summary>Whether the mask and the syllable of a glyph admit it to a match, the first test of <c>skipping_iterator_t::match</c>.</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the glyph may match.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let maskAdmits (m: Matcher) (info: GlyphInfo) (props: GlyphProps) =
        info.Mask &&& m.Mask <> 0u && not (m.PerSyllable && m.Syllable <> 0uy && m.Syllable <> props.Syllable)

    /// <summary>skipping_iterator_t::match (src/hb-ot-layout-gsubgpos.hh:1018-1034).</summary>
    /// <typeparam name="'D">The glyph-data type.</typeparam>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data.</param>
    /// <param name="position">The glyph-data index.</param>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The match verdict.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let verdict<'D when 'D : struct and 'D :> IGlyphData> (m: Matcher) (gdef: Gdef) (data: 'D) (position: int) (info: GlyphInfo) (props: GlyphProps) : MatchVerdict =
        let maskOk = info.Mask &&& m.Mask <> 0u && not (m.PerSyllable && m.Syllable <> 0uy && m.Syllable <> props.Syllable)
        match maySkip m gdef info props with
        | SkipYes -> Skipped
        | SkipNo ->
            match maskOk, data.HasMatchFunc with
            | false, _ -> NotMatched
            | true, false -> Matched
            | true, true -> (match data.Matches (info, props, position) with | true -> Matched | false -> NotMatched)
        | SkipMaybe ->
            match maskOk && data.HasMatchFunc && data.Matches (info, props, position) with
            | true -> Matched
            | false -> Skipped

    /// <summary>skipping_iterator_t::next (src/hb-ot-layout-gsubgpos.hh:1036-1062).</summary>
    /// <typeparam name="'D">The glyph-data type.</typeparam>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data.</param>
    /// <param name="position">The glyph-data index.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The cursor index.</param>
    /// <param name="finish">The exclusive end index.</param>
    /// <returns>The hit, or the unsafe bound of a miss.</returns>
    [<TailCall>]
    let rec next<'D when 'D : struct and 'D :> IGlyphData>
            (m: Matcher) (gdef: Gdef) (data: 'D) (position: int)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) (finish: int) : Probe =
        match index + 1 < finish with
        | false -> Miss finish
        | true ->
            let i = index + 1
            match verdict m gdef data position infos[i] props[i] with
            | Matched -> Hit i
            | NotMatched -> Miss (i + 1)
            | Skipped -> next m gdef data position infos props i finish

    /// <summary>skipping_iterator_t::prev (src/hb-ot-layout-gsubgpos.hh:1064-1090).</summary>
    /// <typeparam name="'D">The glyph-data type.</typeparam>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data.</param>
    /// <param name="position">The glyph-data index.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The cursor index.</param>
    /// <returns>The hit, or the unsafe bound of a miss.</returns>
    [<TailCall>]
    let rec prev<'D when 'D : struct and 'D :> IGlyphData>
            (m: Matcher) (gdef: Gdef) (data: 'D) (position: int)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) : Probe =
        match index > 0 with
        | false -> Miss 0
        | true ->
            let i = index - 1
            match verdict m gdef data position infos[i] props[i] with
            | Matched -> Hit i
            | NotMatched -> Miss (max 1 i - 1)
            | Skipped -> prev m gdef data position infos props i

    /// <summary>The first step of <see cref="next"/> before a match function is asked: the glyphs the matcher skips whatever the
    /// match function says are passed, and the walk stops at the first glyph it does not skip, or at one that depends on the match
    /// function. The same for every match function, so one walk serves every subtable of a lookup at a glyph.</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The cursor index.</param>
    /// <param name="finish">The exclusive end index.</param>
    /// <returns>Where the walk stops.</returns>
    [<TailCall>]
    let rec headNext (m: Matcher) (gdef: Gdef) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) (finish: int) =
        match index + 1 < finish with
        | false -> HeadEnd
        | true ->
            let i = index + 1
            match maySkip m gdef infos[i] props[i] with
            | SkipYes -> headNext m gdef infos props i finish
            | SkipNo -> HeadGlyph i
            | SkipMaybe -> HeadMaybe

    /// <summary>The first step of <see cref="prev"/> before a match function is asked, as <see cref="headNext"/> is for <see cref="next"/>.</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The cursor index.</param>
    /// <returns>Where the walk stops.</returns>
    [<TailCall>]
    let rec headPrev (m: Matcher) (gdef: Gdef) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) =
        match index > 0 with
        | false -> HeadEnd
        | true ->
            let i = index - 1
            match maySkip m gdef infos[i] props[i] with
            | SkipYes -> headPrev m gdef infos props i
            | SkipNo -> HeadGlyph i
            | SkipMaybe -> HeadMaybe

    /// <summary>matcher_t::init (src/hb-ot-layout-gsubgpos.hh:839-853).</summary>
    /// <param name="tableIndex">Zero for GSUB, one for GPOS.</param>
    /// <param name="contextMatch">Whether this is context matching.</param>
    /// <param name="lookupProps">The lookup props.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
    /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
    /// <param name="perSyllable">Whether matching is per syllable.</param>
    /// <param name="syllable">The current syllable.</param>
    /// <returns>The matcher.</returns>
    let matcherFor (tableIndex: int) (contextMatch: bool) (lookupProps: uint32) (lookupMask: uint32) (autoZwnj: bool) (autoZwj: bool) (perSyllable: bool) (syllable: byte) : Matcher =
        { LookupProps = lookupProps
          Mask = match contextMatch with | true -> 0xFFFFFFFFu | false -> lookupMask
          IgnoreZwnj = tableIndex = 1 || (contextMatch && autoZwnj)
          IgnoreZwj = contextMatch || autoZwj
          IgnoreHidden = tableIndex = 1
          PerSyllable = tableIndex = 0 && perSyllable
          Syllable = syllable }

    /// <summary>Sets the syllable a per-syllable lookup matches within, the <c>syllable</c> of <c>matcher_t</c>: a matcher that
    /// is not per syllable is returned as it is.</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="syllable">The syllable.</param>
    /// <returns>The matcher.</returns>
    let withSyllable (m: Matcher) syllable =
        match m.PerSyllable with
        | true -> { m with Syllable = syllable }
        | false -> m

    /// <summary>Sets the syllable a per-syllable lookup matches within to that of the glyph the lookup is applied at,
    /// <c>skipping_iterator_t::reset</c> and <c>reset_back</c> (src/hb-ot-layout-gsubgpos.hh:974-989).</summary>
    /// <param name="m">The matcher.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The index of the glyph the lookup is applied at.</param>
    /// <returns>The matcher.</returns>
    let atSyllable (m: Matcher) (props: ReadOnlySpan<GlyphProps>) index =
        withSyllable m (props[index].Syllable)

    /// <summary>_set_glyph_class (src/hb-ot-layout-gsubgpos.hh:1338-1373), pure.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="glyph">The new glyph.</param>
    /// <param name="classGuess">The guessed class when GDEF classes are absent.</param>
    /// <param name="ligature">Whether the glyph is a ligature.</param>
    /// <param name="isComponent">Whether the glyph is a component.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <returns>The updated glyph properties.</returns>
    let setGlyphClass (gdef: Gdef) (hasGlyphClasses: bool) (glyph: uint32) (classGuess: uint16) (ligature: bool) (isComponent: bool) (props: GlyphProps) : GlyphProps =
        let substitutedProps = props.GlyphClass ||| GlyphPropsBits.Substituted
        let ligatedProps =
            match ligature with
            | true -> (substitutedProps ||| GlyphPropsBits.Ligated) &&& ~~~GlyphPropsBits.Multiplied
            | false -> substitutedProps
        let multipliedProps =
            match isComponent with
            | true -> ligatedProps ||| GlyphPropsBits.Multiplied
            | false -> ligatedProps
        let classBits =
            match hasGlyphClasses, classGuess <> 0us with
            | true, _ -> (multipliedProps &&& GlyphPropsBits.Preserve) ||| Gdef.glyphProps gdef glyph
            | false, true -> (multipliedProps &&& GlyphPropsBits.Preserve) ||| classGuess
            | false, false -> multipliedProps
        { props with GlyphClass = classBits }

    /// <summary>hb_buffer_t::next_serial (src/hb-buffer.hh:238), skipping zero.</summary>
    /// <param name="serial">The current serial.</param>
    /// <returns>The next serial.</returns>
    let nextSerial (serial: byte) : byte =
        match serial + 1uy with
        | 0uy -> serial + 2uy
        | next -> next

    /// <summary>_hb_allocate_lig_id (src/hb-ot-layout.hh:524-531).</summary>
    /// <param name="serial">The current serial.</param>
    /// <returns>The ligature id and the advanced serial.</returns>
    [<TailCall>]
    let rec allocateLigId (serial: byte) : struct (byte * byte) =
        let next = nextSerial serial
        let ligId = next &&& 0x07uy
        match ligId with
        | 0uy -> allocateLigId next
        | _ -> struct (ligId, next)

    /// <summary>hb_ot_apply_context_t::random_number (src/hb-ot-layout-gsubgpos.hh:1274-1279).</summary>
    /// <param name="state">The current random state.</param>
    /// <returns>The next minstd value.</returns>
    let randomNumber (state: uint32) : uint32 =
        (state * 48271u) % 2147483647u

/// <summary>Matches glyph ids against an input array, <c>match_glyph</c>.</summary>
[<Struct>]
type internal GlyphIds =
    val private Ids: ImmutableArray<uint32>
    /// <summary>Creates a glyph-id matcher.</summary>
    /// <param name="ids">The glyph ids.</param>
    new (ids) = { Ids = ids }
    interface IGlyphData with
        member _.HasMatchFunc = true
        member this.Matches (info, _, position) = info.Codepoint = this.Ids[position]

/// <summary>Matches class values against an input array, <c>match_class</c>.</summary>
[<Struct>]
type internal ClassValues =
    val private ClassDef: ClassDef
    val private Values: ImmutableArray<uint32>
    /// <summary>Creates a class-value matcher.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="values">The class values.</param>
    new (classDef, values) = { ClassDef = classDef; Values = values }
    interface IGlyphData with
        member _.HasMatchFunc = true
        member this.Matches (info, _, position) = ClassDef.classOf this.ClassDef info.Codepoint = this.Values[position]

/// <summary>Matches coverage membership against an input array, <c>match_coverage</c>.</summary>
[<Struct>]
type internal CoverageList =
    val private Coverages: ImmutableArray<Coverage>
    /// <summary>Creates a coverage-list matcher.</summary>
    /// <param name="coverages">The coverages.</param>
    new (coverages) = { Coverages = coverages }
    interface IGlyphData with
        member _.HasMatchFunc = true
        member this.Matches (info, _, position) =
            match Coverage.index this.Coverages[position] info.Codepoint with
            | CoverageHit.Covered _ -> true
            | CoverageHit.NotCovered -> false

/// <summary>Matches every glyph, <c>match_always</c>.</summary>
[<Struct>]
type internal MatchAlways =
    interface IGlyphData with
        member _.HasMatchFunc = true
        member _.Matches (_, _, _) = true

/// <summary>A matcher with no match function, <c>match_func == nullptr</c>.</summary>
[<Struct>]
type internal NoMatchFunc =
    interface IGlyphData with
        member _.HasMatchFunc = false
        member _.Matches (_, _, _) = false
