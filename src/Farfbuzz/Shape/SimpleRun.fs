namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Farfbuzz.Aat
open Farfbuzz.OpenType

/// <summary>Which way <see cref="SimpleRun.tryShapePath"/> shaped a run.</summary>
[<Struct>]
type internal SimplePath =
    | LookupFree // the plan has no lookup that can apply: the kernel's arrays are the result
    | NoLookupApplies // lookups can apply to the plan, but the run's glyphs miss every digest
    | SubstitutionUnchanged // the GSUB engine ran and changed nothing
    | SubstitutionFast // the GSUB engine changed the run and the kernel positioned it
    | SubstitutionGeneral // the GSUB engine changed the run and the general positioning ran
    | PositioningGeneral // a GPOS lookup can apply and the run or the font refuses the fast tail: the general positioning ran
    | PositioningFast // the positioning kernel ran on the default advances and the flags were finished in one pass
    | PositioningTail // the positioning kernel ran on the default advances of a run the GSUB engine changed, or of a run the shared tail finished
    | AatSimple // the AAT substitution and the kern or kerx positioning ran on the kernel's arrays and the shared tail finished the run

/// <summary>The fused fast path for simple runs: a horizontal left-to-right run of code points that are plain letters,
/// digits, punctuation and spaces, which the font maps directly, is shaped into its final glyphs, properties and
/// advances in one loop. Every stage of the general pipeline that the loop replaces is the identity or writes a
/// constant on such a run; <c>docs/research/perf-profile-phase4.md</c> ("After the simple-run kernel") lists them.</summary>
[<RequireQualifiedAccess>]
module internal SimpleRun =
    /// <summary>The first code point that is not ASCII, below which no code point carries a rule beyond its category.</summary>
    [<Literal>]
    let private AsciiLimit = 0x80u

    /// <summary>The fraction slash, whose presence the setup-masks stage reads, <c>0x2044</c>.</summary>
    [<Literal>]
    let private FractionSlash = 0x2044u

    /// <summary>The first halfwidth katakana extend character, <c>0xFF9E</c>.</summary>
    [<Literal>]
    let private HalfwidthFirst = 0xFF9Eu

    /// <summary>The last halfwidth katakana extend character, <c>0xFF9F</c>.</summary>
    [<Literal>]
    let private HalfwidthLast = 0xFF9Fu

    /// <summary>The first regional indicator, <c>0x1F1E6</c>.</summary>
    [<Literal>]
    let private RegionalIndicatorFirst = 0x1F1E6u

    /// <summary>The last regional indicator, <c>0x1F1FF</c>.</summary>
    [<Literal>]
    let private RegionalIndicatorLast = 0x1F1FFu

    /// <summary>The first emoji modifier, <c>0x1F3FB</c>.</summary>
    [<Literal>]
    let private EmojiModifierFirst = 0x1F3FBu

    /// <summary>The last emoji modifier, <c>0x1F3FF</c>.</summary>
    [<Literal>]
    let private EmojiModifierLast = 0x1F3FFu

    /// <summary>The first tag character, <c>0xE0020</c>.</summary>
    [<Literal>]
    let private TagFirst = 0xE0020u

    /// <summary>The last tag character, <c>0xE007F</c>.</summary>
    [<Literal>]
    let private TagLast = 0xE007Fu

    /// <summary>The no-break hyphen, <c>U+2011</c>, which a font without a glyph for it replaces with the hyphen.</summary>
    [<Literal>]
    let private NoBreakHyphen = 0x2011u

    /// <summary>Whether a code point is one the continuation rules of <c>hb_set_unicode_props</c> look at: a halfwidth
    /// katakana extend character, a regional indicator, an emoji modifier or a tag.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True for the code points that can become grapheme continuations.</returns>
    let private isRuleCodepoint (codepoint: uint32) =
        (codepoint >= HalfwidthFirst && codepoint <= HalfwidthLast)
        || (codepoint >= RegionalIndicatorFirst && codepoint <= RegionalIndicatorLast)
        || (codepoint >= EmojiModifierFirst && codepoint <= EmojiModifierLast)
        || (codepoint >= TagFirst && codepoint <= TagLast)

    /// <summary>The Unicode properties of a simple code point: its general category and nothing else, so that it is no
    /// mark, no default ignorable, no continuation, no joiner and no space with a fallback width.</summary>
    /// <param name="ascii">The precomputed properties of U+0000 to U+00FF.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The packed properties, or none when the code point is not simple.</returns>
    let private simpleUnicode (ascii: ImmutableArray<struct (uint16 * ScratchFlags)>) (codepoint: uint32) =
        match codepoint < AsciiLimit with
        | true ->
            let struct (unicode, _) = ascii[int codepoint]
            ValueSome unicode
        | false ->
            let struct (unicode, _) = UnicodeProps.computeLow codepoint
            match unicode &&& ~~~UnicodeProps.GenCatMask = 0us && codepoint <> FractionSlash && not (isRuleCodepoint codepoint) with
            | true -> ValueSome unicode
            | false -> ValueNone

    /// <summary>The glyph class a glyph ends with: the synthesized base class when the plan synthesizes classes, else the
    /// GDEF class when the face has glyph classes, else none, <c>_hb_ot_layout_set_glyph_props</c> followed by
    /// <c>hb_synthesize_glyph_classes</c> on a glyph that is no non-spacing mark.</summary>
    /// <param name="synthesize">Whether the plan synthesizes glyph classes.</param>
    /// <param name="hasClasses">Whether the face has GDEF glyph classes.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The glyph class bits.</returns>
    let private classOf synthesize hasClasses (gdef: Gdef) (glyph: uint32) =
        match synthesize, hasClasses with
        | true, _ -> GlyphPropsBits.Base
        | false, true -> Gdef.glyphProps gdef glyph
        | false, false -> 0us

    /// <summary>The per-glyph decision of the kernel, reading the font's accelerators once.</summary>
    [<Struct>]
    type internal SimpleGlyphs =
        val Font: Font
        val Cmap: CmapAccel
        val Low: ImmutableArray<uint32>
        val Gdef: Gdef
        val Mtx: Mtx
        val Ascii: ImmutableArray<struct (uint16 * ScratchFlags)>
        val HasClasses: bool
        val Synthesize: bool
        val HasMetrics: bool
        val Plain: bool
        val Varied: ImmutableArray<int>
        val UpemHalf: int
        val Embolden: bool
        val XMult: int64
        val AcceptsMisses: bool
        val NotFound: uint32

        /// <summary>Creates the decision for a font, for a kernel that gives up at the first code point the font does not map.</summary>
        /// <param name="font">The font.</param>
        /// <param name="synthesize">Whether the plan synthesizes glyph classes.</param>
        new (font: Font, synthesize: bool) = SimpleGlyphs(font, synthesize, false, 0u)

        /// <summary>Creates the decision for a font, for a kernel that may take a plain code point the font does not map as the
        /// not-found glyph, as the normalizer's short-circuit step does for a miss that has no decomposition and is no space.</summary>
        /// <param name="font">The font.</param>
        /// <param name="synthesize">Whether the plan synthesizes glyph classes.</param>
        /// <param name="acceptsMisses">Whether a plain miss is taken.</param>
        /// <param name="notFound">The not-found glyph of the buffer.</param>
        new (font: Font, synthesize: bool, acceptsMisses: bool, notFound: uint32) =
            let mtx = font.Ot.Hmtx.Value
            let gdef = font.Ot.Gdef.Value
            let hasMetrics = Hmtx.hasData mtx
            let embolden = font.XStrength <> 0 && not font.EmboldenInPlace
            { Font = font
              Cmap = font.Ot.Cmap.Value
              Low = Cmap.lowGlyphs font.Ot.Cmap.Value
              Gdef = gdef
              Mtx = mtx
              Ascii = UnicodeProps.latinTable
              HasClasses = Gdef.hasGlyphClasses gdef
              Synthesize = synthesize
              HasMetrics = hasMetrics
              Plain = hasMetrics && not embolden && not font.Variations.Coords.HasNonZero
              Varied = (match embolden with | true -> ImmutableArray<int>.Empty | false -> Font.hvarAdvances font)
              UpemHalf = Font.emScaleX font (font.Face.Upem / 2)
              Embolden = embolden
              XMult = font.XMult
              AcceptsMisses = acceptsMisses
              NotFound = notFound }

        /// <summary>Reads the default horizontal advance of a glyph, <c>hb_ot_position_default</c>'s horizontal branch.</summary>
        /// <param name="glyph">The glyph.</param>
        /// <param name="metrics">The bytes of the horizontal metrics.</param>
        /// <returns>The advance.</returns>
        member this.AdvanceOf (glyph: uint32, metrics: ReadOnlySpan<byte>) =
            match this.Plain with
            | true -> Font.emScaleBy this.XMult (Hmtx.advanceIn &this.Mtx metrics glyph)
            | false ->
                match glyph < uint32 this.Varied.Length with
                | true -> Font.emScaleBy this.XMult this.Varied[int glyph]
                | false -> Font.hAdvanceFrom this.Font this.Mtx this.HasMetrics this.UpemHalf this.Embolden glyph

        interface ISimpleAdvances with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Advance (glyph, metrics) = this.AdvanceOf (glyph, metrics)

        /// <summary>Classifies one glyph the font has chosen for a code point of the given Unicode properties.</summary>
        /// <param name="glyph">The glyph.</param>
        /// <param name="unicode">The packed Unicode properties.</param>
        /// <param name="metrics">The bytes of the horizontal metrics.</param>
        /// <returns>The glyph, or none when its class is a mark.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.ClassOf (glyph: uint32, unicode: uint16, metrics: ReadOnlySpan<byte>) =
            let klass = classOf this.Synthesize this.HasClasses this.Gdef glyph
            match klass &&& GlyphPropsBits.Mark <> 0us with
            | true -> ValueNone
            | false ->
                ValueSome
                    { SimpleProps =
                        { UnicodeProps = unicode
                          GlyphClass = klass
                          LigProps = 0uy
                          Syllable = 0uy
                          ShaperCategory = 0uy
                          ShaperAuxiliary = 0uy
                          GlyphIndex = glyph }
                      SimpleAdvance = this.AdvanceOf (glyph, metrics) }

        /// <summary>Classifies one code point, the decision of the kernel's first loop.</summary>
        /// <param name="codepoint">The code point.</param>
        /// <param name="metrics">The bytes of the horizontal metrics.</param>
        /// <returns>The glyph, or none when the code point does not qualify as simple.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GlyphOf (codepoint: uint32, metrics: ReadOnlySpan<byte>) =
            match simpleUnicode this.Ascii codepoint, this.NominalOf codepoint with
            | ValueSome unicode, ValueSome glyph -> this.ClassOf (glyph, unicode, metrics)
            | ValueNone, _ -> ValueNone
            | ValueSome unicode, ValueNone -> this.MissOf (codepoint, unicode, metrics)

        /// <summary>The glyph the cmap gives a code point, <see cref="Cmap.nominalGlyph"/>, read from the face's table of U+0000 to U+20FF
        /// held in the decision when the code point is in it, so that a letter of a script below U+2100 costs one read.</summary>
        /// <param name="codepoint">The code point.</param>
        /// <returns>The glyph, or none.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.NominalOf (codepoint: uint32) =
            match codepoint < uint32 this.Low.Length with
            | true ->
                match this.Low[int codepoint] with
                | 0u -> ValueNone
                | glyph -> ValueSome glyph
            | false -> Cmap.nominalGlyph this.Cmap codepoint

        /// <summary>Classifies a simple code point the font does not map: when the kernel takes misses, one with no decomposition that is
        /// no space separator and not the no-break hyphen is the not-found glyph, <c>decompose_current_character</c> when nothing else
        /// applies to it.</summary>
        /// <param name="codepoint">The code point.</param>
        /// <param name="unicode">The packed Unicode properties.</param>
        /// <param name="metrics">The bytes of the horizontal metrics.</param>
        /// <returns>The not-found glyph, or none when the code point is refused.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.MissOf (codepoint: uint32, unicode: uint16, metrics: ReadOnlySpan<byte>) =
            match this.AcceptsMisses, UnicodeProps.isSpaceValue unicode, codepoint = NoBreakHyphen with
            | false, _, _ -> ValueNone
            | true, true, _ -> ValueNone
            | true, false, true -> ValueNone
            | true, false, false ->
                match Unicode.decompose codepoint with
                | ValueSome _ -> ValueNone
                | ValueNone -> this.ClassOf (this.NotFound, unicode, metrics)

        interface ISimpleClassifier with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Classify (glyph, unicode, metrics) = this.ClassOf (glyph, unicode, metrics)

        interface ISimpleGlyphs with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Glyph (codepoint, metrics) = this.GlyphOf (codepoint, metrics)

    /// <summary>The decision of the kernel on a run it reads backwards: a code point the mirroring of right-to-left text
    /// would change is not simple, <c>hb_ot_rotate_chars</c>.</summary>
    [<Struct>]
    type internal MirroredGlyphs =
        val Inner: SimpleGlyphs

        /// <summary>Creates the decision for a font.</summary>
        /// <param name="font">The font.</param>
        /// <param name="synthesize">Whether the plan synthesizes glyph classes.</param>
        new (font: Font, synthesize: bool) = { Inner = SimpleGlyphs(font, synthesize) }

        interface ISimpleGlyphs with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Glyph (codepoint, metrics) =
                match Unicode.mirroring codepoint = codepoint with
                | false -> ValueNone
                | true -> this.Inner.GlyphOf (codepoint, metrics)

    /// <summary>Whether every user feature is global, so that the setup-masks stage has nothing to write.</summary>
    /// <param name="features">The user features.</param>
    /// <param name="index">The next feature.</param>
    /// <returns>True when no feature has a range.</returns>
    [<TailCall>]
    let rec private allGlobal (features: ImmutableArray<Feature>) index =
        match index >= features.Length with
        | true -> true
        | false ->
            match features[index].Start = Feature.GlobalStart && features[index].End = Feature.GlobalEnd with
            | true -> allGlobal features (index + 1)
            | false -> false

    /// <summary>Whether a plan's shaper and features leave a line to the kernel, whatever the direction: the default shaper with
    /// its composed-diacritics normalization, no legacy kern, kerx, morx or tracking, and no ranged user feature.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>True when the kernel may try a line of the plan.</returns>
    let private isSimpleShaping (plan: OtShapePlan) =
        (match plan.Shaper.Kind with
         | DefaultShaper -> true
         | DumberShaper -> false
         | ArabicShaper -> false
         | ThaiShaper -> false
         | HangulShaper -> false
         | HebrewShaper -> false
         | IndicShaper -> false
         | KhmerShaper -> false
         | MyanmarShaper -> false
         | MyanmarZawgyiShaper -> false
         | UseShaper -> false)
        && Normalize.mode plan = NormalizationComposedDiacritics
        && not plan.ApplyKern
        && not plan.ApplyKerx
        && not plan.ApplyMorx
        && not plan.ApplyTrak
        && allGlobal plan.UserFeatures 0

    /// <summary>Whether a plan is the Hangul shaper's with nothing that would keep the kernel from a line without a jamo: no legacy kern,
    /// kerx, morx or tracking, and no ranged user feature. The kernel gives each code point the glyph the font maps to it (the
    /// normalization mode of the shaper is none, so the glyph of a mapped code point is taken as it is,
    /// <c>hb-ot-shape-normalize.cc</c>'s <c>always_short_circuit</c>), refuses a mark, a decomposable code point the font does not map
    /// and any glyph of a mark class, so that the shaper's zero-width-marks mode (none, where the default shaper's is late by GDEF)
    /// has no mark to act on; a line holding a jamo is left to the general route
    /// (<see cref="Hangul.anyJamo"/>).</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>True when the kernel may try a line of the plan that holds no jamo.</returns>
    let private isHangulShaping (plan: OtShapePlan) =
        (match plan.Shaper.Kind with
         | HangulShaper -> true
         | DefaultShaper -> false
         | DumberShaper -> false
         | ArabicShaper -> false
         | ThaiShaper -> false
         | HebrewShaper -> false
         | IndicShaper -> false
         | KhmerShaper -> false
         | MyanmarShaper -> false
         | MyanmarZawgyiShaper -> false
         | UseShaper -> false)
        && not plan.ApplyKern
        && not plan.ApplyKerx
        && not plan.ApplyMorx
        && not plan.ApplyTrak
        && allGlobal plan.UserFeatures 0

    /// <summary>Whether a plan and a run qualify for the kernel as a Hangul line: a successful run, left to right in a script that is not
    /// right to left, the plan of <see cref="isHangulShaping"/>, and no jamo in the run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the kernel may try the run as a Hangul line.</returns>
    let private isHangulPlan (plan: OtShapePlan) (run: GlyphRun) =
        run.Successful
        && run.SegmentProperties.Direction = Direction.Ltr
        && plan.Props.Direction = Direction.Ltr
        && Script.horizontalDirection run.SegmentProperties.Script <> Direction.Rtl
        && isHangulShaping plan
        && not (Hangul.anyJamo (run.Infos.AsSpan()) 0)

    /// <summary>Whether a plan and a run qualify for the kernel apart from the code points: a successful run, left to
    /// right in a script that is not right to left, the default shaper with its composed-diacritics normalization, no
    /// legacy kern, kerx, morx or tracking, and no ranged user feature.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the kernel may try the run.</returns>
    let internal isSimplePlan (plan: OtShapePlan) (run: GlyphRun) =
        run.Successful
        && run.SegmentProperties.Direction = Direction.Ltr
        && plan.Props.Direction = Direction.Ltr
        && Script.horizontalDirection run.SegmentProperties.Script <> Direction.Rtl
        && isSimpleShaping plan

    /// <summary>The mask every glyph carries after the flag propagation: the global mask cut down to the defined glyph
    /// flags, <c>hb_propagate_flags</c> on a run whose glyphs all share one mask.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The final mask.</returns>
    let internal finalMask (plan: OtShapePlan) (run: GlyphRun) =
        let andMask = Flags.andMaskOf run.Flags
        let mask = plan.Map.GlobalMask
        match run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel <> BufferFlags.Default with
        | false -> mask &&& andMask
        | true ->
            let mask =
                match mask &&& uint32 GlyphFlags.UnsafeToBreak <> 0u with
                | true -> mask &&& ~~~(uint32 GlyphFlags.SafeToInsertTatweel)
                | false -> mask
            let mask =
                match mask &&& uint32 GlyphFlags.SafeToInsertTatweel <> 0u with
                | true -> mask ||| uint32 GlyphFlags.UnsafeToBreak ||| uint32 GlyphFlags.UnsafeToConcat
                | false -> mask
            mask &&& andMask

    /// <summary>Builds the final run of a simple run no lookup can touch.</summary>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the final mask.</param>
    /// <returns>The shaped run, with the limits and serial of a finished shaping.</returns>
    let private finished (run: GlyphRun) (arrays: SimpleRunArrays) =
        { run with
            Infos = arrays.KernelInfos
            Props = arrays.KernelProps
            Positions = arrays.KernelPositions
            ContentType = GlyphContent
            HavePositions = true
            ScratchFlags = ScratchFlags.Default
            MaxLen = GlyphRun.MaxLenDefault
            MaxOps = GlyphRun.MaxOpsDefault
            Serial = 0u }

    /// <summary>Resets the limits and the serial of a shaped run, <c>Execute.run</c>'s tail.</summary>
    /// <param name="shaped">The shaped run or its error.</param>
    /// <returns>The shaped run with the default limits, or the error.</returns>
    let private reset (shaped: Result<GlyphRun, ShapeError>) =
        match shaped with
        | Error error -> Error error
        | Ok result -> Ok { result with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }

    /// <summary>The scratch flags the lookup engine may leave on a run that the fast tail still accepts: none, since every
    /// flag the later stages read (space fallback, default ignorables, variation-selector fallback, GPOS attachments) must
    /// be unset for their stage to be the identity.</summary>
    [<Literal>]
    let private AcceptedScratch = ScratchFlags.Default

    /// <summary>Positions and finishes a run the GSUB engine changed through the general positioning, the tail of every run
    /// the fast tail refuses. It is a function of its own so that it runs only for a run that is refused: an eagerly bound
    /// result would position every substituted run twice.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <returns>The general path and the shaped run.</returns>
    let private generalTail otPlan font target substituted =
        struct (SubstitutionGeneral, reset (OtShaper.fromSubstituted otPlan font target substituted))

    /// <summary>Positions and finishes a run the GSUB engine changed, when its tail is the identity: the run is still
    /// successful, no scratch flag is set, no glyph is a mark and no glyph carries a Unicode property beyond its general
    /// category (so none is a mark, hidden or default ignorable); the positions are then the default advances of the
    /// substituted glyphs, and the shared tail propagates the flags. Any other run takes the general positioning. A run in
    /// which a ligature formed is read the same way: the ligature glyph, the merged clusters and the unsafe-to-break and
    /// unsafe-to-concat flags of matched and failed matches are all in the substituted run, which the shared tail reads
    /// as the general positioning does, and a ligature that left a mark glyph, a scratch flag or a Unicode property
    /// beyond the category fails the conditions above.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let internal afterSubstitution (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (substituted: GlyphRun) =
        let target = run.SegmentProperties.Direction
        match substituted.Successful && substituted.ScratchFlags = AcceptedScratch with
        | false -> generalTail otPlan font target substituted
        | true ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match substituted.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default with
            | true ->
                match Sealed.simpleFinish (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask (Flags.andMaskOf substituted.Flags) with
                | ValueNone -> generalTail otPlan font target substituted
                | ValueSome (struct (infos, positions)) ->
                    struct (SubstitutionFast, reset (Ok (GlyphRun.withDirection target { substituted with Infos = infos; Positions = positions; HavePositions = true })))
            | false ->
                match Sealed.simpleAdvances (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask with
                | ValueNone -> generalTail otPlan font target substituted
                | ValueSome positions ->
                    struct (SubstitutionFast, reset (Ok (OtShaper.afterPosition otPlan font target { substituted with Positions = positions; HavePositions = true })))

    /// <summary>Builds the final run of a simple run whose arrays the lookups did not change, from arrays that carry the global
    /// mask: every glyph gets the final mask, in place, the arrays being those the kernel made in this call.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <returns>The shaped run.</returns>
    let private settled (otPlan: OtShapePlan) (run: GlyphRun) (arrays: SimpleRunArrays) =
        Sealed.setAllMasks arrays.KernelInfos (finalMask otPlan run)
        finished run arrays

    /// <summary>Builds the final run of a simple run the positioning kernel finished, with the limits and serial of a
    /// finished shaping.</summary>
    /// <param name="run">The input run.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions.</param>
    /// <returns>The shaped run.</returns>
    let private finishedWith (run: GlyphRun) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) =
        { run with
            Infos = infos
            Props = props
            Positions = positions
            ContentType = GlyphContent
            HavePositions = true
            ScratchFlags = ScratchFlags.Default
            MaxLen = GlyphRun.MaxLenDefault
            MaxOps = GlyphRun.MaxOpsDefault
            Serial = 0u }

    /// <summary>Finishes a simple run whose pair lookups the lean loop applied in place to the kernel's arrays: the arrays are
    /// the positioned run, and the flags are finished as <see cref="positionKernelled"/> finishes them.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, positioned and flagged.</param>
    /// <param name="maxOps">The operation budget the lean loop left.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private positionedLean (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) (maxOps: int) =
        let target = run.SegmentProperties.Direction
        let tail () =
            let positioned =
                { GlyphRun.enter run with
                    Infos = arrays.KernelInfos
                    Props = arrays.KernelProps
                    Positions = arrays.KernelPositions
                    HavePositions = true
                    ContentType = GlyphContent
                    MaxOps = maxOps }
            struct (PositioningTail, reset (Ok (OtShaper.afterPosition otPlan font target positioned)))
        match run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default with
        | false -> tail ()
        | true ->
            match Sealed.finishFlags arrays.KernelInfos (Flags.andMaskOf run.Flags) with
            | false -> tail ()
            | true -> struct (PositioningFast, Ok (finishedWith run arrays.KernelInfos arrays.KernelProps arrays.KernelPositions))

    /// <summary>Positions a run the GSUB engine did not change with the positioning kernel, on the kernel's default advances,
    /// and finishes it. Every stage of <c>OtPosition.positionComposed</c> that follows the GPOS pass is the identity for such a
    /// run, so the run is finished with the shared tail alone, or with its fused form when the flags can be finished in one
    /// pass:
    /// the default positions are those of <c>positionDefault</c> (the kernel writes them from the same metrics and no glyph
    /// has the space fallback flag); <c>GposApply.positionStart</c> clears the attach bits of positions whose <c>Var</c> is
    /// zero; <c>zeroMarkWidthsByGdef</c> and <c>zeroWidthDefaultIgnorables</c> act on marks and default ignorables, of which
    /// the run has none; <c>finishOffsets</c> propagates attachments, of which single and pair adjustment make none and
    /// which a cursive or mark lookup records in the scratch flags, so that a run whose positioning set a scratch flag takes the
    /// general path, and corrects the offsets of a slanted font, which the caller refuses; <c>fallbackMarkPosition</c> acts on
    /// marks only. The tail is then <c>hb_propagate_flags</c>: the cut to the defined flags, and a union over each cluster,
    /// which is the identity when every glyph is its own cluster.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <param name="pre">The run the kernel's arrays make, without positions.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private positionKernelledGeneral (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) (pre: GlyphRun) =
        let target = run.SegmentProperties.Direction
        let positioned = Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs { pre with Positions = arrays.KernelPositions; HavePositions = true }
        match positioned.Successful && positioned.ScratchFlags = pre.ScratchFlags, run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default with
        | false, _ -> struct (PositioningGeneral, reset (OtShaper.fromSubstituted otPlan font target pre))
        | true, false -> struct (PositioningTail, reset (Ok (OtShaper.afterPosition otPlan font target positioned)))
        | true, true ->
            match Sealed.finishFlags positioned.Infos (Flags.andMaskOf run.Flags) with
            | false -> struct (PositioningTail, reset (Ok (OtShaper.afterPosition otPlan font target positioned)))
            | true -> struct (PositioningFast, Ok (finishedWith run positioned.Infos positioned.Props positioned.Positions))

    /// <summary>Positions a run the GSUB engine did not change: the pair lookups are applied in place to the kernel's arrays by the
    /// lean loop when the run is one it applies to, and by the positioning kernel on the default advances otherwise.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <param name="pre">The run the kernel's arrays make, without positions.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private positionKernelled (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) (pre: GlyphRun) =
        match Engine.positionOwnedUnmarked plan.GposPairs font arrays.KernelInfos arrays.KernelPositions run.Flags run.ClusterLevel pre.MaxOps with
        | ValueSome maxOps -> positionedLean otPlan font run arrays maxOps
        | ValueNone -> positionKernelledGeneral plan otPlan font run arrays pre

    /// <summary>Positions a run the GSUB engine changed through the general positioning kernel, on the default advances of its glyphs,
    /// and finishes it with the shared tail; a run whose positioning failed or set a scratch flag takes the general positioning.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <param name="advances">The default positions of its glyphs.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private positionChanged (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (substituted: GlyphRun) (advances: ImmutableArray<GlyphPosition>) =
        let target = run.SegmentProperties.Direction
        let positioned = Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs { substituted with Positions = advances; HavePositions = true }
        match positioned.Successful && positioned.ScratchFlags = substituted.ScratchFlags with
        | false -> struct (PositioningGeneral, reset (OtShaper.fromSubstituted otPlan font target substituted))
        | true -> struct (PositioningTail, reset (Ok (OtShaper.afterPosition otPlan font target positioned)))

    /// <summary>Positions a run the GSUB engine changed, on default advances that <c>Sealed.leanAdvances</c> accepted (every glyph carries the
    /// global mask and none is a mark or carries a Unicode property beyond its category), with the lean loop in the engine's own arrays,
    /// which no one else holds: the pair lookups write the advances and the glyph flags in place and the flags are finished as
    /// <see cref="positionedLean"/> finishes them, a run in which two neighbours share a cluster through the shared tail. A run the lean loop
    /// does not apply to, or a buffer that wants the tatweel rule, takes the general positioning, which the lean loop leaves untouched.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <param name="advances">The default positions of its glyphs, in an array of their own.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private positionLeanChanged (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (substituted: GlyphRun) (advances: ImmutableArray<GlyphPosition>) =
        match run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default with
        | false -> positionChanged plan otPlan font run substituted advances
        | true ->
            match Engine.positionOwnedUnmarked plan.GposPairs font substituted.Infos advances run.Flags run.ClusterLevel substituted.MaxOps with
            | ValueNone -> positionChanged plan otPlan font run substituted advances
            | ValueSome maxOps ->
                let target = run.SegmentProperties.Direction
                let positioned = { substituted with Positions = advances; HavePositions = true; MaxOps = maxOps }
                match Sealed.finishFlags positioned.Infos (Flags.andMaskOf run.Flags) with
                | false -> struct (PositioningTail, reset (Ok (OtShaper.afterPosition otPlan font target positioned)))
                | true -> struct (PositioningTail, reset (Ok (GlyphRun.withDirection target positioned)))

    /// <summary>Positions a run the GSUB engine changed with the positioning kernel, on the default advances of its new
    /// glyphs, when the preconditions of <see cref="afterSubstitution"/> hold and the font is not slanted, and finishes it with
    /// the shared tail; any other run takes the general positioning.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let internal substitutedKernelled (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (substituted: GlyphRun) =
        let target = run.SegmentProperties.Direction
        let general () = struct (PositioningGeneral, reset (OtShaper.fromSubstituted otPlan font target substituted))
        match substituted.Successful && substituted.ScratchFlags = AcceptedScratch && font.SlantXY = 0f with
        | false -> general ()
        | true ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match Sealed.leanAdvances (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask otPlan.Map.GlobalMask with
            | ValueSome advances -> positionLeanChanged plan otPlan font run substituted advances
            | ValueNone ->
                match Sealed.simpleAdvances (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask with
                | ValueNone -> general ()
                | ValueSome advances -> positionChanged plan otPlan font run substituted advances

    /// <summary>Finishes a simple run some lookup might touch through the general kernels: the GSUB engine runs on the state the
    /// kernel produced when a GSUB lookup can apply, and the positioning kernel runs on the default advances when a GPOS lookup
    /// can apply to the glyphs as they stand after the substitution; the kernel's arrays stand when neither changed anything.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <param name="substitutes">Whether a GSUB lookup can apply.</param>
    /// <param name="admission">The look at the glyphs the substitution admission made.</param>
    /// <param name="positions">Whether a GPOS lookup can apply to the kernel's glyphs.</param>
    /// <param name="limits">The length limit and the operation budget the lookups start with.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private withEntered (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) (substitutes: bool) (admission: RunAdmission) (positions: bool) (limits: struct (int * int)) =
        let struct (maxLen, maxOps) = limits
        let pre =
            { run with
                MaxLen = maxLen
                MaxOps = maxOps
                Serial = 0u
                ScratchFlags = ScratchFlags.Default
                Infos = arrays.KernelInfos
                Props = arrays.KernelProps
                Positions = ImmutableArray.Empty
                HavePositions = false
                ContentType = GlyphContent }
        let substituted =
            match substitutes with
            | true -> Engine.substituteAdmitted otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission pre
            | false -> pre
        let target = run.SegmentProperties.Direction
        let unchanged = obj.ReferenceEquals(substituted, pre)
        let positions = match unchanged with | true -> positions | false -> Sealed.anyGlyphIn plan.GposCovered (substituted.Infos.AsSpan())
        match unchanged, positions, font.SlantXY = 0f with
        | true, false, _ -> struct (SubstitutionUnchanged, Ok (settled otPlan run arrays))
        | true, true, true -> positionKernelled plan otPlan font run arrays pre
        | true, true, false -> struct (PositioningGeneral, reset (OtShaper.fromSubstituted otPlan font target substituted))
        | false, false, _ -> afterSubstitution otPlan font run substituted
        | false, true, _ -> substitutedKernelled plan otPlan font run substituted

    /// <summary>Finishes a simple run some lookup might touch: a run that only GPOS pair lookups can touch, whose font is not
    /// slanted, goes to the lean loop, which positions the kernel's arrays in place; any other run goes through the general
    /// kernels, as does a run the lean loop does not apply to.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <param name="limits">The length limit and the operation budget the lookups start with.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private withLookups (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) (limits: struct (int * int)) =
        let struct (_, budget) = limits
        let globalMask = otPlan.Map.GlobalMask
        let struct (substitutes, admission) =
            match arrays.Covered with
            | true -> Engine.admit otPlan font plan.GsubGlyphLookups globalMask run.Flags (arrays.KernelInfos.AsSpan())
            | false -> struct (false, { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty })
        let positions = Sealed.anyGlyphIn plan.GposCovered (arrays.KernelInfos.AsSpan())
        match substitutes || positions with
        | false -> struct (NoLookupApplies, Ok (settled otPlan run arrays))
        | true ->
            match substitutes || font.SlantXY <> 0f with
            | true -> withEntered plan otPlan font run arrays substitutes admission positions limits
            | false ->
                match Engine.positionOwnedUnmarked plan.GposPairs font arrays.KernelInfos arrays.KernelPositions run.Flags run.ClusterLevel budget with
                | ValueNone -> withEntered plan otPlan font run arrays substitutes admission positions limits
                | ValueSome maxOps -> positionedLean otPlan font run arrays maxOps

    /// <summary>Runs the kernel on a run whose plan qualifies, giving up at the first code point that is not simple. Read forwards, a plain
    /// code point the font does not map is the run's not-found glyph, as <c>LineShape</c>'s kernel takes it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <param name="mirrored">Whether the run is read backwards, so that a code point right-to-left text would mirror is not simple.</param>
    /// <returns>The path taken and the shaped run, or none when the general pipeline must run.</returns>
    let private shapeSimple (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (mirrored: bool) =
        let lookupFree = plan.GsubCovered.IsEmpty && plan.GposCovered.IsEmpty
        let mask = match lookupFree with | true -> finalMask otPlan run | false -> otPlan.Map.GlobalMask
        let metrics = font.Ot.Hmtx.Value.Table.Span
        let made =
            match mirrored with
            | false -> Sealed.simpleRun (SimpleGlyphs(font, otPlan.FallbackGlyphClasses, true, run.NotFound)) mask (run.Infos.AsSpan()) metrics plan.GsubCovered
            | true -> Sealed.simpleRun (MirroredGlyphs(font, otPlan.FallbackGlyphClasses)) mask (run.Infos.AsSpan()) metrics plan.GsubCovered
        match made with
        | ValueNone -> ValueNone
        | ValueSome arrays ->
            match lookupFree with
            | true -> ValueSome (struct (LookupFree, Ok (finished run arrays)))
            | false -> ValueSome (withLookups plan otPlan font run arrays (struct (GlyphRun.enteredMaxLen run, GlyphRun.enteredMaxOps run)))

    /// <summary>Whether a plan applies the AAT tables, and nothing else that could touch a line, to a line the kernel may shape:
    /// a horizontal left-to-right run under the default or dumber shaper with its composed-diacritics normalization, morx, kern
    /// or kerx applying, no GSUB, GPOS or tracking applying, and no ranged user feature.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the kernel may try the line and finish it through the AAT stages.</returns>
    let private isAatSimplePlan (plan: ShapePlan) (otPlan: OtShapePlan) (run: GlyphRun) =
        run.Successful
        && run.SegmentProperties.Direction = Direction.Ltr
        && otPlan.Props.Direction = Direction.Ltr
        && Script.horizontalDirection run.SegmentProperties.Script <> Direction.Rtl
        && (match otPlan.Shaper.Kind with
            | DefaultShaper -> true
            | DumberShaper -> true
            | ArabicShaper -> false
            | ThaiShaper -> false
            | HangulShaper -> false
            | HebrewShaper -> false
            | IndicShaper -> false
            | KhmerShaper -> false
            | MyanmarShaper -> false
            | MyanmarZawgyiShaper -> false
            | UseShaper -> false)
        && Normalize.mode otPlan = NormalizationComposedDiacritics
        && (otPlan.ApplyMorx || otPlan.ApplyKern || otPlan.ApplyKerx)
        && not otPlan.ApplyGpos
        && not otPlan.ApplyTrak
        && plan.GsubCovered.IsEmpty
        && plan.GposCovered.IsEmpty
        && allGlobal otPlan.UserFeatures 0

    /// <summary>The Unicode property bits a glyph of the substituted run may carry beyond its general category: the AAT deleted
    /// mark of the glyph a ligature leaves behind, which the later removal reads.</summary>
    [<Literal>]
    let private AatAllowedUnicode = UnicodeProps.GenCatMask ||| UnicodeProps.CfAatDeleted ||| UnicodeProps.Hidden

    /// <summary>The tail of a line the AAT stages positioned: when no glyph was deleted and no flag needs the tatweel rule, the
    /// flags are cut in place on the arrays this call made, which is the whole of <c>hb_propagate_flags</c> when no two glyphs
    /// share a cluster and its propagation over the clusters in place when some do; a line whose only scratch flag is the deleted glyph
    /// first has those glyphs removed, the one stage of the shared tail a simple line does anything in; any other line takes the shared tail.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="positioned">The positioned run, whose arrays this call made.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private aatTail (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (positioned: GlyphRun) =
        let plain = positioned.ScratchFlags &&& ~~~AatApply.aatHasDeleted = ScratchFlags.Default && run.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default
        let cleaned =
            match plain && positioned.ScratchFlags &&& AatApply.aatHasDeleted <> ScratchFlags.Default with
            | true -> AatLayout.removeDeletedGlyphs positioned
            | false -> positioned
        match plain && cleaned.Successful with
        | false -> struct (AatSimple, reset (Ok (OtShaper.afterPosition otPlan font run.SegmentProperties.Direction positioned)))
        | true ->
            let andMask = Flags.andMaskOf run.Flags
            match Sealed.finishFlags cleaned.Infos andMask with
            | true -> ()
            | false -> Sealed.propagateFlagsOwned cleaned.Infos andMask false
            struct (AatSimple, Ok { finishedWith run cleaned.Infos cleaned.Props cleaned.Positions with ScratchFlags = cleaned.ScratchFlags })

    /// <summary>Positions the kernel's own arrays of an AAT line, <c>hb_ot_position</c> without GPOS: the kerx table first, then the
    /// legacy kern table, which writes the kerned pairs into the arrays this call made instead of copying them.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, whose arrays this call made.</param>
    /// <returns>The positioned run.</returns>
    let private positionAat (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let kerxed =
            match otPlan.ApplyKerx with
            | true -> AatLayout.positionOwned otPlan font run
            | false -> run
        match otPlan.ApplyKern with
        | true -> Kern.applyKernOwned otPlan font kerxed
        | false -> kerxed

    /// <summary>Finishes a line the kernel shaped through the AAT stages, <c>hb_ot_substitute_pre</c>'s AAT branch,
    /// <c>hb_ot_position</c>'s kern and kerx branch and the shared tail: the kernel's arrays are the run the general pipeline has
    /// before the AAT substitution, the substitution runs on them, the default advances are rewritten when the glyphs changed,
    /// the kern or kerx pass runs on them, and a run that the identity stages between would not leave alone takes the
    /// general positioning from the substituted run.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The input run.</param>
    /// <param name="arrays">The kernel's arrays, whose infos carry the global mask.</param>
    /// <returns>The path taken and the shaped run.</returns>
    let private finishAat (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) (arrays: SimpleRunArrays) =
        let target = run.SegmentProperties.Direction
        let pre =
            { run with
                MaxLen = GlyphRun.enteredMaxLen run
                MaxOps = GlyphRun.enteredMaxOps run
                Serial = 0u
                ScratchFlags = ScratchFlags.Default
                Infos = arrays.KernelInfos
                Props = arrays.KernelProps
                Positions = ImmutableArray.Empty
                HavePositions = false
                ContentType = GlyphContent }
        let struct (substituted, edited) =
            match otPlan.ApplyMorx with
            | true -> AatLayout.substituteOwned otPlan font (otPlan.UserFeatures.AsSpan()) pre
            | false -> struct (pre, false)
        let general () = struct (SubstitutionGeneral, reset (OtShaper.fromSubstituted otPlan font target substituted))
        let scratch = substituted.ScratchFlags &&& ~~~AatApply.aatHasDeleted
        match substituted.Successful && scratch = ScratchFlags.Default && font.SlantXY = 0f with
        | false -> general ()
        | true ->
            let sameGlyphs = not edited
            let advances =
                match sameGlyphs with
                | true -> ValueSome arrays.KernelPositions
                | false ->
                    let metrics = font.Ot.Hmtx.Value.Table.Span
                    let allowed = match substituted.ScratchFlags &&& AatApply.aatHasDeleted = ScratchFlags.Default with | true -> UnicodeProps.GenCatMask | false -> AatAllowedUnicode
                    Sealed.simpleAdvances (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics GlyphPropsBits.Mark ~~~allowed
            match advances with
            | ValueNone -> general ()
            | ValueSome positions ->
                let positioned = positionAat otPlan font { substituted with Positions = positions; HavePositions = true }
                match positioned.Successful && positioned.ScratchFlags &&& ~~~AatApply.aatHasDeleted = ScratchFlags.Default with
                | false -> general ()
                | true -> aatTail otPlan font run positioned

    /// <summary>Shapes a line of a plan that applies the AAT tables: the kernel makes the glyphs, properties and default advances in
    /// one loop, giving up at the first code point that is not simple, and the AAT stages finish the line.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The path taken and the shaped run, or none when the general pipeline must run.</returns>
    let private shapeAat (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match isAatSimplePlan plan otPlan run with
        | false -> ValueNone
        | true ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match Sealed.simpleRun (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) otPlan.Map.GlobalMask (run.Infos.AsSpan()) metrics plan.GsubCovered with
            | ValueNone -> ValueNone
            | ValueSome arrays -> ValueSome (finishAat otPlan font run arrays)

    /// <summary>Whether a plan and a run qualify for the kernel once the run is read backwards: a right-to-left run of a script that
    /// is left to right natively, which <c>hb_ensure_native_direction</c> reverses and shapes left to right, with the default
    /// shaper, no feature that the mirroring of <c>hb_ot_rotate_chars</c> would tag, and the other conditions of
    /// <see cref="isSimplePlan"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the kernel may try the run reversed.</returns>
    let private isFlippedPlan (plan: OtShapePlan) (run: GlyphRun) =
        run.Successful
        && run.SegmentProperties.Direction = Direction.Rtl
        && plan.Props.Direction = Direction.Rtl
        && Script.horizontalDirection run.SegmentProperties.Script = Direction.Ltr
        && plan.RtlmMask &&& ~~~plan.Map.GlobalMask = 0u
        && isSimpleShaping plan

    /// <summary>Shapes a right-to-left run of a left-to-right script as the general pipeline does: the glyphs are reversed, the run is
    /// shaped left to right, and the direction is restored, <c>hb_ensure_native_direction</c> and the end of
    /// <c>hb_ot_shape_internal</c>. Every code point is a whole grapheme, which the kernel only accepts, so reversing the graphemes is
    /// reversing the glyphs.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The path taken and the shaped run, or none when the general pipeline must run.</returns>
    let private shapeFlipped (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let flipped =
            { run with
                Infos = Sealed.reverseRange (run.Infos.AsSpan()) 0 run.Infos.Length
                SegmentProperties = { run.SegmentProperties with Direction = Direction.Ltr } }
        match shapeSimple plan otPlan font flipped true with
        | ValueNone -> ValueNone
        | ValueSome (struct (path, Error error)) -> ValueSome (struct (path, Error error))
        | ValueSome (struct (path, Ok shaped)) -> ValueSome (struct (path, Ok (GlyphRun.withDirection run.SegmentProperties.Direction shaped)))

    /// <summary>The scratch flags a normalized line may carry into the kernel's tails: the grapheme continuations of the marks the
    /// normalizer composed away, which no stage after the normalizer reads.</summary>
    [<Literal>]
    let private CarriedScratch = ScratchFlags.HasContinuations

    /// <summary>Hands a normalized line to the kernel's tails when it is a line the kernel would have accepted had its text been the
    /// normalized text: the default shaper, a successful left-to-right run, every glyph with the global mask, no property beyond its
    /// category and a class that is no mark. The arrays are those <see cref="Sealed.simpleRun"/> makes of that text, the lookups and
    /// the positioning run on them as they do for a line of the kernel, with the limits the lookups have after the normalizer, and the
    /// flags the normalizer's passes left are carried to the shaped run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction of the input run, which the shaped run takes back.</param>
    /// <param name="normalized">The normalized run.</param>
    /// <returns>The shaped run, or none when the stages after the normalizer must run.</returns>
    let internal handOff (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (normalized: GlyphRun) =
        match normalized.Successful
              && normalized.ScratchFlags &&& ~~~CarriedScratch = ScratchFlags.Default
              && normalized.SegmentProperties.Direction = Direction.Ltr
              && Script.horizontalDirection normalized.SegmentProperties.Script <> Direction.Rtl
              && isSimpleShaping otPlan with
        | false -> ValueNone
        | true ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match Sealed.simpleRunOfNormalized (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) otPlan.Map.GlobalMask ~~~UnicodeProps.GenCatMask (normalized.Infos.AsSpan()) (normalized.Props.AsSpan()) metrics plan.GsubCovered with
            | ValueNone -> ValueNone
            | ValueSome arrays ->
                let struct (_, shaped) =
                    match plan.GsubCovered.IsEmpty && plan.GposCovered.IsEmpty with
                    | true -> struct (LookupFree, Ok (settled otPlan normalized arrays))
                    | false -> withLookups plan otPlan font normalized arrays (struct (normalized.MaxLen, normalized.MaxOps))
                match shaped with
                | Error error -> ValueSome (Error error)
                | Ok result -> ValueSome (Ok (GlyphRun.withDirection target { result with ScratchFlags = result.ScratchFlags ||| normalized.ScratchFlags }))

    /// <summary>Whether a substituted run is one the marked line's positioning reads exactly as <c>OtPosition.position</c> does: a
    /// successful horizontal run of a plan that applies GPOS alone, with no space fallback, no fallback mark positioning and no mark
    /// offset adjustment, which is the plan of the Latin, Hebrew and Thai lines that keep their marks.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <returns>True when the lean positioning applies.</returns>
    let internal isMarkedLine (otPlan: OtShapePlan) (substituted: GlyphRun) =
        substituted.Successful
        && Direction.isHorizontal substituted.SegmentProperties.Direction
        && otPlan.ApplyGpos
        && not otPlan.ApplyKern
        && not otPlan.ApplyTrak
        && not otPlan.ApplyKerx
        && not otPlan.FallbackMarkPositioning
        && not otPlan.AdjustMarkPositioningWhenZeroing
        && substituted.ScratchFlags &&& ScratchFlags.HasSpaceFallback = ScratchFlags.Default

    /// <summary>Zeroes the advances of marks when the plan does so at the stage asked for.</summary>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="stage">The stage, early before the GPOS pass or late after it.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the mark advances zeroed, or the run itself.</returns>
    let private zeroedMarks (otPlan: OtShapePlan) stage (run: GlyphRun) =
        match otPlan.ZeroMarks && otPlan.Shaper.ZeroWidthMarks = stage with
        | false -> run
        | true -> { run with Positions = Sealed.zeroMarkAdvances (run.Props.AsSpan()) (run.Positions.AsSpan()) }

    /// <summary>Positions a substituted run that keeps its marks, from its default advances: the zero-width marks of the shaper's stage,
    /// the GPOS lookups through the plan's glyph sets, the default-ignorable widths, the attachment offsets, and the reversal of a
    /// backward run. A run the GPOS pass could not finish takes the general positioning from the substituted run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <param name="advances">The default positions of its glyphs.</param>
    /// <returns>The shaped run.</returns>
    let private markedFrom (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (substituted: GlyphRun) (advances: ImmutableArray<GlyphPosition>) =
        let defaulted = { substituted with Positions = advances; HavePositions = true }
        let early = zeroedMarks otPlan ZeroWidthMarksByGdefEarly defaulted
        let positioned = Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs early
        match positioned.Successful with
        | false -> reset (OtShaper.fromSubstituted otPlan font target substituted)
        | true ->
            let late = zeroedMarks otPlan ZeroWidthMarksByGdefLate positioned
            let finished = GposApply.finishOffsets font (OtPosition.zeroWidthDefaultIgnorables late)
            let ordered =
                match Direction.isBackward finished.SegmentProperties.Direction with
                | true -> GlyphRun.reverse finished
                | false -> finished
            reset (Ok (OtShaper.afterPosition otPlan font target ordered))

    /// <summary>Positions a substituted run that keeps its marks as <c>OtPosition.position</c> does, on one working copy of the
    /// positions instead of ten stage passes: the default advances read in one loop (the attach chains start clear, so
    /// <c>GPOS::position_start</c> has nothing to clear), then <see cref="markedFrom"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="target">The direction to restore.</param>
    /// <param name="substituted">The run after the GSUB engine.</param>
    /// <returns>The shaped run.</returns>
    let private positionMarked (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (target: Direction) (substituted: GlyphRun) =
        match isMarkedLine otPlan substituted with
        | false -> reset (OtShaper.fromSubstituted otPlan font target substituted)
        | true ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match Sealed.simpleAdvances (SimpleGlyphs(font, otPlan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics 0us 0us with
            | ValueNone -> reset (OtShaper.fromSubstituted otPlan font target substituted)
            | ValueSome advances -> markedFrom plan otPlan font target substituted advances

    /// <summary>Shapes a run through the general pipeline with its front half lean, and says whether the kernel's tails took the line:
    /// the normalizer's result is handed to them when the line has no mark left, and goes through the stages that follow the
    /// normalizer otherwise. The result is that of <see cref="Execute.runGeneral"/> in every field.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>Whether the kernel's tails shaped the line, and the shaped run.</returns>
    let internal shapeGeneralPath (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let entered = GlyphRun.enter run
        let target = run.SegmentProperties.Direction
        let normalized = OtShaper.normalised otPlan font entered
        match handOff plan otPlan font target normalized with
        | ValueSome shaped -> struct (true, shaped)
        | ValueNone ->
            match OtSubstitute.afterNormalize otPlan font normalized with
            | Error error -> struct (false, Error error)
            | Ok substituted -> struct (false, positionMarked plan otPlan font target substituted)

    /// <summary>Shapes a run through the general pipeline with its front half lean, <see cref="shapeGeneralPath"/> without the path.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="otPlan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The shaped run.</returns>
    let shapeGeneral (plan: ShapePlan) (otPlan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let struct (_, shaped) = shapeGeneralPath plan otPlan font run
        shaped

    /// <summary>Shapes a simple run and says which way it went: the kernel runs when the plan and the run qualify, and
    /// gives up at the first code point that is not simple.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The path taken and the shaped run, or none when the general pipeline must run.</returns>
    let tryShapePath (plan: ShapePlan) (font: Font) (run: GlyphRun) =
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            match isSimplePlan otPlan run, isHangulPlan otPlan run, isFlippedPlan otPlan run with
            | true, _, _ -> shapeSimple plan otPlan font run false
            | false, true, _ -> shapeSimple plan otPlan font run false
            | false, false, true -> shapeFlipped plan otPlan font run
            | false, false, false -> shapeAat plan otPlan font run

    /// <summary>Shapes a simple run: the kernel runs when the plan and the run qualify, and gives up at the first code
    /// point that is not simple.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, not empty.</param>
    /// <returns>The shaped run, or none when the general pipeline must run.</returns>
    let tryShape (plan: ShapePlan) (font: Font) (run: GlyphRun) =
        match tryShapePath plan font run with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, shaped)) -> ValueSome shaped

    /// <summary>The kernel's decision for one line, built once: the glyph reader of a plan that shapes a line without any
    /// lookup, so that every fragment of the line is shaped by the kernel alone.</summary>
    [<NoEquality; NoComparison>]
    type internal SimpleWindow = { Glyphs: SimpleGlyphs }

    /// <summary>How a window of text compares with the shaped glyphs it should have produced.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type internal WindowVerdict =
        | WindowAgrees // the kernel's glyphs, clusters and positions equal the shaped glyphs they are compared with
        | WindowDiffers // some glyph, cluster or position differs
        | WindowRefused // the kernel refuses some code point of the window: the general pipeline must shape it

    /// <summary>Prepares the window kernel for a line, when the plan qualifies for the kernel and no lookup can apply, so
    /// that the shaped fragment of any window is its kernel arrays and nothing else.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run whose segment properties and success every fragment shares.</param>
    /// <returns>The window kernel, or none when fragments need the general route.</returns>
    let internal window (plan: ShapePlan) (font: Font) (run: GlyphRun) =
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            match isSimplePlan otPlan run && plan.GsubCovered.IsEmpty && plan.GposCovered.IsEmpty with
            | false -> ValueNone
            | true -> ValueSome { Glyphs = SimpleGlyphs(font, otPlan.FallbackGlyphClasses) }

    /// <summary>Compares the kernel's output for a window of text with the shaped glyphs from an index, glyph by glyph:
    /// the glyph, the cluster and the four position fields, as <c>hb_buffer_diff</c> compares them with no fuzz.</summary>
    /// <param name="glyphs">The per-glyph decision.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="source">The window of text.</param>
    /// <param name="infos">The shaped glyphs the window maps to.</param>
    /// <param name="positions">The shaped positions the window maps to.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The verdict.</returns>
    [<TailCall>]
    let rec private windowFrom<'G when 'G : struct and 'G :> ISimpleGlyphs> (glyphs: 'G) (metrics: ReadOnlySpan<byte>) (source: ReadOnlySpan<GlyphInfo>) (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match index >= source.Length with
        | true -> WindowAgrees
        | false ->
            let info = source[index]
            match glyphs.Glyph(info.Codepoint, metrics) with
            | ValueNone -> WindowRefused
            | ValueSome simple ->
                let shaped = infos[index]
                let position = positions[index]
                let same =
                    shaped.Codepoint = simple.SimpleProps.GlyphIndex
                    && shaped.Cluster = info.Cluster
                    && position.XAdvance = simple.SimpleAdvance
                    && position.YAdvance = 0
                    && position.XOffset = 0
                    && position.YOffset = 0
                match same with
                | true -> windowFrom glyphs metrics source infos positions (index + 1)
                | false -> WindowDiffers

    /// <summary>Compares a window of text with the shaped glyphs from an offset, without shaping a fragment: the verdict
    /// the fragment's shaped run would give when appended to the reconstruction at that offset.</summary>
    /// <param name="window">The window kernel of the line.</param>
    /// <param name="font">The font.</param>
    /// <param name="text">The text.</param>
    /// <param name="start">The first text index of the window.</param>
    /// <param name="finish">The end text index of the window.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="offset">The first shaped glyph of the window.</param>
    /// <returns>The verdict.</returns>
    let internal compareWindow (window: SimpleWindow) (font: Font) (text: GlyphRun) (start: int) (finish: int) (shaped: GlyphRun) (offset: int) =
        match offset + (finish - start) > shaped.Infos.Length with
        | true -> WindowDiffers
        | false ->
            let metrics = font.Ot.Hmtx.Value.Table.Span
            match windowFrom window.Glyphs metrics (text.Infos.AsSpan(start, finish - start)) (shaped.Infos.AsSpan(offset, finish - start)) (shaped.Positions.AsSpan(offset, finish - start)) 0 with
            | WindowAgrees -> WindowAgrees
            | WindowDiffers -> WindowDiffers
            | WindowRefused -> WindowRefused
