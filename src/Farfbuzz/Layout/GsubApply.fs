namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Numerics
open System.Runtime.CompilerServices

/// <summary>The outcome of trying one GSUB subtable on the current glyph.</summary>
[<Struct>]
type internal GsubMatch =
    | GsubMiss
    | GsubReplace of info: GlyphInfo * props: GlyphProps
    | GsubAlternateRandom of info: GlyphInfo * props: GlyphProps * randomState: uint32
    | GsubMultiple of info: GlyphInfo * components: ImmutableArray<uint32> * klass: uint16 * baseProps: GlyphProps * ligId: byte
    | GsubDelete
    | GsubLigature of ligatures: ImmutableArray<Ligature> * secondDigest: Digest

/// <summary>Applying GSUB single, multiple and alternate subtables, <c>SingleSubst</c>, <c>MultipleSubst</c>,
/// <c>AlternateSubst</c>.</summary>
[<RequireQualifiedAccess>]
module internal GsubApply =
    /// <summary>The alternate-substitution value that randomises, <c>HB_OT_MAP_MAX_VALUE</c>.</summary>
    [<Literal>]
    let private MaxValue = 0xFFu

    /// <summary>Builds a single-substitution replacement, <c>replace_glyph</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="glyph">The new glyph.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <returns>The replacement.</returns>
    let private replace gdef hasGlyphClasses glyph info props =
        let props = Matching.setGlyphClass gdef hasGlyphClasses glyph 0us false false props
        GsubReplace ({ info with Codepoint = glyph }, props)

    /// <summary>The substituted glyph of a delta single substitution, <c>SingleSubstFormat1_3::apply</c>'s modulo.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="delta">The delta.</param>
    /// <param name="glyphMask">The modulo mask.</param>
    /// <param name="info">The current glyph.</param>
    /// <returns>The substituted glyph, or none.</returns>
    let singleDeltaGlyph coverage delta glyphMask info =
        match Coverage.index coverage info.Codepoint with
        | CoverageHit.NotCovered -> ValueNone
        | CoverageHit.Covered _ -> ValueSome ((info.Codepoint + delta) &&& glyphMask)

    /// <summary>The substituted glyph of a list single substitution, <c>SingleSubstFormat2_4::apply</c>.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="substitutes">The substitutes.</param>
    /// <param name="info">The current glyph.</param>
    /// <returns>The substituted glyph, or none.</returns>
    let singleListGlyph coverage (substitutes: ImmutableArray<uint32>) info =
        match Coverage.index coverage info.Codepoint with
        | CoverageHit.NotCovered -> ValueNone
        | CoverageHit.Covered index ->
            match index < substitutes.Length with
            | false -> ValueNone
            | true -> ValueSome substitutes[index]

    /// <summary>Applies a single-substitution subtable, <c>SingleSubst::apply</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <returns>The match outcome.</returns>
    let private singleSub gdef hasGlyphClasses subtable info props =
        match subtable with
        | SingleDelta (coverage, delta, glyphMask) ->
            match singleDeltaGlyph coverage delta glyphMask info with
            | ValueNone -> GsubMiss
            | ValueSome glyph -> replace gdef hasGlyphClasses glyph info props
        | SingleList (coverage, substitutes) ->
            match singleListGlyph coverage substitutes info with
            | ValueNone -> GsubMiss
            | ValueSome glyph -> replace gdef hasGlyphClasses glyph info props
        | _ -> GsubMiss

    /// <summary>Applies a multiple-substitution subtable, <c>MultipleSubst::apply</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="coverage">The coverage.</param>
    /// <param name="sequences">The sequences.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <returns>The match outcome.</returns>
    let private multipleSub gdef hasGlyphClasses coverage (sequences: ImmutableArray<ImmutableArray<uint32>>) info props =
        match Coverage.index coverage info.Codepoint with
        | CoverageHit.NotCovered -> GsubMiss
        | CoverageHit.Covered index ->
            let sequence = match index < sequences.Length with | true -> sequences[index] | false -> ImmutableArray<uint32>.Empty
            match sequence.Length with
            | 1 -> replace gdef hasGlyphClasses sequence[0] info props
            | 0 -> GsubDelete
            | _ ->
                let klass = match GlyphPropsBits.isLigature props with | true -> GlyphPropsBits.Base | false -> 0us
                let ligId = GlyphPropsBits.ligId props
                GsubMultiple (info, sequence, klass, props, ligId)

    /// <summary>Emits the chosen alternate, <c>AlternateSet::apply</c>'s tail.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="alternates">The alternate set.</param>
    /// <param name="altIndex">The chosen alternate index, one-based.</param>
    /// <param name="nextRandom">The next random state.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <param name="randomUsed">Whether the random feature chose the alternate.</param>
    /// <returns>The match outcome.</returns>
    let private alternatePick gdef hasGlyphClasses (alternates: ImmutableArray<uint32>) altIndex nextRandom info props randomUsed =
        match altIndex > uint32 alternates.Length || altIndex = 0u with
        | true -> GsubMiss
        | false ->
            let glyph = alternates[int (altIndex - 1u)]
            let props = Matching.setGlyphClass gdef hasGlyphClasses glyph 0us false false props
            match randomUsed with
            | true -> GsubAlternateRandom ({ info with Codepoint = glyph }, props, nextRandom)
            | false -> GsubReplace ({ info with Codepoint = glyph }, props)

    /// <summary>Resolves the alternate index, randomising when the value is <c>HB_OT_MAP_MAX_VALUE</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="alternates">The alternate set.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The match outcome.</returns>
    let private alternateEmit gdef hasGlyphClasses alternates (lookupMask: uint32) random info props randomState =
        let shift = BitOperations.TrailingZeroCount lookupMask
        let altIndex = (lookupMask &&& (info : GlyphInfo).Mask) >>> shift
        match altIndex = MaxValue && random with
        | true ->
            let next = Matching.randomNumber randomState
            alternatePick gdef hasGlyphClasses alternates (next % uint32 alternates.Length + 1u) next info props true
        | false -> alternatePick gdef hasGlyphClasses alternates altIndex randomState info props false

    /// <summary>Applies an alternate-substitution subtable, <c>AlternateSubst::apply</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="coverage">The coverage.</param>
    /// <param name="alternateSets">The alternate sets.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="info">The current glyph.</param>
    /// <param name="props">The current glyph properties.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The match outcome.</returns>
    let private alternateSub gdef hasGlyphClasses coverage (alternateSets: ImmutableArray<ImmutableArray<uint32>>) lookupMask random info props randomState =
        match Coverage.index coverage info.Codepoint with
        | CoverageHit.NotCovered -> GsubMiss
        | CoverageHit.Covered index when index >= alternateSets.Length -> GsubMiss
        | CoverageHit.Covered index -> alternateEmit gdef hasGlyphClasses alternateSets[index] lookupMask random info props randomState

    /// <summary>The longest context a ligature may match, <c>HB_MAX_CONTEXT_LENGTH</c> (src/hb-limits.hh:58).</summary>
    [<Literal>]
    let private MaxContextLength = 64

    /// <summary>Whether every component after the first is a mark, for the ligate-input classification.</summary>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="components">The components after the first glyph.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <returns>True when every component is a mark.</returns>
    [<TailCall>]
    let rec private allComponentsAreMarks
            (matcher: Matcher) (gdef: Gdef) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (components: ImmutableArray<uint32>) (componentIndex: int) (matchStart: int) (count: int) =
        match componentIndex < count with
        | false -> true
        | true ->
            match Matching.next matcher gdef (GlyphIds components) (componentIndex - 1) infos props matchStart infos.Length with
            | Probe.Miss _ -> true
            | Probe.Hit hitIndex ->
                match GlyphPropsBits.isMark props[hitIndex] with
                | false -> false
                | true -> allComponentsAreMarks matcher gdef infos props components (componentIndex + 1) hitIndex count

    /// <summary>The ligature's first-glyph properties: the ligature id and component count, and the Mn-to-Lo
    /// category change, <c>ligate_input</c>'s pre-replace rewrite.</summary>
    /// <param name="isLigature">Whether the ligature is a real ligature.</param>
    /// <param name="ligId">The ligature id.</param>
    /// <param name="total">The total component count.</param>
    /// <param name="firstProps">The first glyph's properties.</param>
    /// <returns>The rewritten properties.</returns>
    let private ligatureFirstProps (isLigature: bool) (ligId: byte) (total: int) (firstProps: GlyphProps) =
        match isLigature with
        | false -> firstProps
        | true ->
            let withLig = GlyphPropsBits.setLigPropsForLigature ligId (byte total) firstProps
            match UnicodeProps.generalCategory withLig = GeneralCategory.NonSpacingMark with
            | true -> UnicodeProps.setGeneralCategory GeneralCategory.OtherLetter withLig
            | false -> withLig

    /// <summary>The outcome of a ligature whose components matched: the ligature glyph, its properties and the facts of the ligation,
    /// <c>Ligature::apply</c> after <c>match_input</c>. Kept out of line so that the ligatures that do not match, which are most of those
    /// tried, do not pay for the locals of a ligation.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lig">The ligature.</param>
    /// <param name="firstProps">The properties of the first glyph.</param>
    /// <param name="allMarks">Whether every component after the first is a mark.</param>
    /// <param name="serial">The buffer serial.</param>
    /// <param name="count">The number of components, the first included.</param>
    /// <param name="matchEnd">The end of the match.</param>
    /// <param name="total">The number of components the match covers.</param>
    /// <returns>The outcome.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private ligatureFormed gdef hasGlyphClasses (lig: Ligature) (firstProps: GlyphProps) allMarks serial count matchEnd total =
        let isBaseLigature = GlyphPropsBits.isBaseGlyph firstProps && allMarks
        let isMarkLigature = GlyphPropsBits.isMark firstProps && allMarks
        let isLigature = not isBaseLigature && not isMarkLigature
        let struct (ligId, nextSerial) =
            match isLigature with
            | true -> Matching.allocateLigId serial
            | false -> struct (0uy, serial)
        let klass = if isLigature then GlyphPropsBits.Ligature else 0us
        let ligPropsProps = ligatureFirstProps isLigature ligId total firstProps
        let finalProps = Matching.setGlyphClass gdef hasGlyphClasses lig.LigGlyph klass true false ligPropsProps
        let firstLigId = GlyphPropsBits.ligId firstProps
        let firstNumComponents = int (GlyphPropsBits.ligNumComps firstProps)
        LigatureLigate
            (LigateFacts(lig.LigGlyph, finalProps, lig.Components, ligId, isLigature, isMarkLigature, matchEnd, count, nextSerial),
             firstLigId, firstNumComponents, firstNumComponents)

    /// <summary>Applies one ligature of a ligature set, <c>Ligature::apply</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="lig">The ligature.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The current glyph index.</param>
    /// <param name="serial">The buffer serial.</param>
    /// <returns>The outcome.</returns>
    let ligatureApply gdef hasGlyphClasses matcher (lig: Ligature) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx serial =
        match GsubTables.ligCount lig with
        | 0 -> LigatureMiss
        | 1 ->
            let props = Matching.setGlyphClass gdef hasGlyphClasses lig.LigGlyph 0us false false props[idx]
            LigatureReplace ({ infos[idx] with Codepoint = lig.LigGlyph }, props)
        | count when count > MaxContextLength -> LigatureMiss
        | count ->
            let struct (matched, matchEnd, total) =
                ContextApply.matchInput matcher gdef infos props outInfos outProps outInfos.Length idx count lig.Components
            match matched with
            | false -> LigatureUnsafeToConcat matchEnd
            | true -> ligatureFormed gdef hasGlyphClasses lig props[idx] (allComponentsAreMarks matcher gdef infos props lig.Components 1 idx count) serial count matchEnd total

    /// <summary>Resolves the ligature set of a ligature subtable for the current glyph,
    /// <c>LigatureSubstFormat1::apply</c>'s coverage lookup.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="sets">The ligature sets.</param>
    /// <param name="secondDigest">The subtable's second-glyph digest.</param>
    /// <param name="info">The current glyph.</param>
    /// <returns>The match outcome.</returns>
    let private ligatureSub coverage (sets: ImmutableArray<ImmutableArray<Ligature>>) secondDigest info =
        match Coverage.index coverage info.Codepoint with
        | CoverageHit.NotCovered -> GsubMiss
        | CoverageHit.Covered index ->
            match index < sets.Length with
            | false -> GsubMiss
            | true -> GsubLigature (sets[index], secondDigest)

    /// <summary>Tries one subtable on the current glyph, <c>SingleSubst::apply</c> and friends.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature drives the alternates.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <returns>The match outcome.</returns>
    let trySubtable gdef hasGlyphClasses lookupMask random subtable (cursor: inref<RunCursor>) =
        match subtable with
        | SingleDelta _ | SingleList _ -> singleSub gdef hasGlyphClasses subtable cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
        | Multiple (coverage, sequences) -> multipleSub gdef hasGlyphClasses coverage sequences cursor.Input[cursor.Index] cursor.InputProps[cursor.Index]
        | Alternate (coverage, alternateSets) -> alternateSub gdef hasGlyphClasses coverage alternateSets lookupMask random cursor.Input[cursor.Index] cursor.InputProps[cursor.Index] cursor.RandomState
        | LigatureSubst (coverage, sets, secondDigest) -> ligatureSub coverage sets secondDigest cursor.Input[cursor.Index]
        | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> GsubMiss

    /// <summary>Tries one subtable on the glyph at an index, <see cref="trySubtable"/> for a caller that holds the
    /// arrays rather than a cursor.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature drives the alternates.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The match outcome.</returns>
    let trySubtableAt gdef hasGlyphClasses lookupMask random subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index randomState =
        match subtable with
        | SingleDelta _ | SingleList _ -> singleSub gdef hasGlyphClasses subtable infos[index] props[index]
        | Multiple (coverage, sequences) -> multipleSub gdef hasGlyphClasses coverage sequences infos[index] props[index]
        | Alternate (coverage, alternateSets) -> alternateSub gdef hasGlyphClasses coverage alternateSets lookupMask random infos[index] props[index] randomState
        | LigatureSubst (coverage, sets, secondDigest) -> ligatureSub coverage sets secondDigest infos[index]
        | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> GsubMiss
