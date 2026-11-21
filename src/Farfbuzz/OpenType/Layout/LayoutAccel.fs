namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>One parsed subtable and its digest, <c>hb_accelerate_subtables_context_t::hb_applicable_t</c>.</summary>
[<Struct>]
type internal SubtableAccel<'S> =
    { Subtable: 'S
      SubtableDigest: Digest }

/// <summary>One parsed lookup and its digests, <c>hb_ot_layout_lookup_accelerator_t</c>.</summary>
type internal LookupAccel<'S> =
    { LookupType: int
      Props: uint32
      IsReverse: bool
      Subtables: ImmutableArray<SubtableAccel<'S>>
      LookupDigest: Digest
      SecondDigest: Digest }

/// <summary>A parsed lookup before its digests are built, the lookup header plus its raw subtables.</summary>
[<Struct>]
type internal ParsedLookup<'S> =
    { LookupType: int
      Props: uint32
      IsReverse: bool
      Subtables: ImmutableArray<'S> }

/// <summary>Building a lookup's digests from its parsed subtables, <c>hb_ot_layout_lookup_accelerator_t::create</c>.</summary>
[<RequireQualifiedAccess>]
module internal LayoutAccel =
    /// <summary>Folds the subtables of a lookup into its two digests.</summary>
    /// <param name="subtables">The subtables with their subtable digests.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="lookupDigest">The lookup digest so far.</param>
    /// <param name="second">The second digest so far.</param>
    /// <returns>The lookup digest and the second digest.</returns>
    [<TailCall>]
    let rec private foldSubtables
            (subtables: ReadOnlySpan<SubtableAccel<'S>>)
            (secondDigest: 'S -> Digest)
            (index: int)
            (lookupDigest: Digest)
            (second: Digest)
            : struct (Digest * Digest) =
        match index < subtables.Length with
        | false -> struct (lookupDigest, second)
        | true ->
            let accel = subtables[index]
            foldSubtables subtables secondDigest (index + 1)
                (Digest.union lookupDigest accel.SubtableDigest)
                (Digest.union second (secondDigest accel.Subtable))

    /// <summary>Builds a lookup accelerator from a parsed lookup, <c>hb_ot_layout_lookup_accelerator_t::create</c>.</summary>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <param name="lookup">The parsed lookup.</param>
    /// <returns>The lookup accelerator.</returns>
    let build (subtableDigest: 'S -> Digest) (secondDigest: 'S -> Digest) (lookup: ParsedLookup<'S>) : LookupAccel<'S> =
        let subtables =
            Sealed.map (fun subtable -> { Subtable = subtable; SubtableDigest = subtableDigest subtable }) (lookup.Subtables.AsSpan())
        let struct (lookupDigest, second) = foldSubtables (subtables.AsSpan()) secondDigest 0 Digest.empty Digest.empty
        { LookupType = lookup.LookupType
          Props = lookup.Props
          IsReverse = lookup.IsReverse
          Subtables = subtables
          LookupDigest = lookupDigest
          SecondDigest = second }

/// <summary>The shallow subtable reader that computes a lookup's digests from raw bytes, before the subtable parsers land.</summary>
[<RequireQualifiedAccess>]
module internal SubtableDigest =
    /// <summary>The extension lookup type for GSUB, <c>SubstLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private GsubExtension = 7

    /// <summary>The extension lookup type for GPOS, <c>PosLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private GposExtension = 9

    /// <summary>The byte width of a 16-bit offset or value.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The byte width of a 24-bit offset or value.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>Whether a lookup type is a GSUB or GPOS context type.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True for context.</returns>
    let private isContext (lookupType: int) (isGsub: bool) : bool =
        (isGsub && lookupType = 5) || (not isGsub && lookupType = 7)

    /// <summary>Whether a lookup type is a GSUB or GPOS chained-context type.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True for chained context.</returns>
    let private isChain (lookupType: int) (isGsub: bool) : bool =
        (isGsub && lookupType = 6) || (not isGsub && lookupType = 8)

    /// <summary>Whether a lookup type is a GSUB or GPOS extension type.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True for extension.</returns>
    let private isExtension (lookupType: int) (isGsub: bool) : bool =
        (isGsub && lookupType = GsubExtension) || (not isGsub && lookupType = GposExtension)

    /// <summary>Whether a format uses 24-bit offsets, <c>MediumTypes</c>.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True for a medium-type format.</returns>
    let private isMedium (lookupType: int) (format: int) (isGsub: bool) : bool =
        match isGsub, lookupType with
        | true, 1 -> format >= 3
        | true, 2 | true, 3 | true, 4 | true, 8 -> format >= 2
        | true, 5 | true, 6 -> format >= 4
        | false, 1 | false, 2 -> format >= 3
        | false, 3 | false, 4 | false, 5 | false, 6 -> format >= 2
        | false, 7 | false, 8 -> format >= 4
        | _ -> false

    /// <summary>The byte width of a subtable's offsets for its format.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The offset width.</returns>
    let private offsetWidth (lookupType: int) (format: int) (isGsub: bool) : int =
        match isMedium lookupType format isGsub with
        | true -> Width24
        | false -> Width16

    /// <summary>The byte width of a subtable's coverage offset, <c>LOffsetTo</c>: 32-bit for medium formats.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The coverage-offset width.</returns>
    let private coverageWidth lookupType format isGsub =
        match isMedium lookupType format isGsub with
        | true -> 4
        | false -> Width16

    /// <summary>Reads a subtable's coverage offset field, <c>LOffsetTo</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <param name="width">The coverage-offset width.</param>
    /// <returns>The offset value.</returns>
    let private readCoverageOffset (table: ReadOnlySpan<byte>) offset width =
        match width with
        | 4 -> Binary.u32 table offset
        | _ -> uint32 (Binary.u16 table offset)

    /// <summary>Resolves a relative offset, yielding none for the Null (zero) offset.</summary>
    /// <param name="baseOffset">The base the offset is relative to.</param>
    /// <param name="offset">The offset value.</param>
    /// <returns>The absolute offset, or none when the offset is Null.</returns>
    let private follow (baseOffset: int) (offset: uint32) : int voption =
        match offset with
        | 0u -> ValueNone
        | _ -> ValueSome (baseOffset + int offset)

    /// <summary>Finds the absolute offset of a subtable's primary coverage, unwrapping extensions.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The absolute coverage offset, or none for a Null coverage.</returns>
    [<TailCall>]
    let rec private coverageAt (table: ReadOnlySpan<byte>) (o: int) (lookupType: int) (isGsub: bool) : int voption =
        match isExtension lookupType isGsub with
        | true ->
            let innerType = int (Binary.u16 table (o + 2))
            match follow o (Binary.u32 table (o + 4)) with
            | ValueNone -> ValueNone
            | ValueSome inner -> coverageAt table inner innerType isGsub
        | false ->
            let format = int (Binary.u16 table o)
            let w = offsetWidth lookupType format isGsub
            let covW = coverageWidth lookupType format isGsub
            match isContext lookupType isGsub && (format = 3 || format = 6) with
            | true -> follow o (Coverage.readWide table (o + 6) w)
            | false ->
                match isChain lookupType isGsub && format = 3 with
                | true ->
                    let backtrackCount = int (Binary.u16 table (o + 2))
                    follow o (Coverage.readWide table (o + 6 + backtrackCount * w) w)
                | false -> follow o (readCoverageOffset table (o + 2) covW)

    /// <summary>The coverage digest of a subtable, <c>get_coverage ().collect_coverage</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The digest.</returns>
    let private coverageDigest (table: ReadOnlySpan<byte>) (o: int) (lookupType: int) (isGsub: bool) : Digest =
        match coverageAt table o lookupType isGsub with
        | ValueNone -> Digest.empty
        | ValueSome cov -> Coverage.digest (Coverage.parse table cov)

    /// <summary>The digest of a coverage reached through a relative offset field, empty for a Null offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The relative coverage offset.</param>
    /// <returns>The digest.</returns>
    let private covAt (table: ReadOnlySpan<byte>) (o: int) (offsetValue: uint32) : Digest =
        match follow o offsetValue with
        | ValueNone -> Digest.empty
        | ValueSome cov -> Coverage.digest (Coverage.parse table cov)

    /// <summary>A class definition reached through a relative offset field, Null for a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The relative class-definition offset.</param>
    /// <returns>The class definition.</returns>
    let private classDefAt (table: ReadOnlySpan<byte>) (o: int) (offsetValue: uint32) : ClassDef =
        match follow o offsetValue with
        | ValueNone -> ClassDefNull
        | ValueSome at -> ClassDef.parse table at

    /// <summary>Whether a digest is full.</summary>
    /// <param name="digest">The digest.</param>
    /// <returns>True when every mask is all bits set.</returns>
    let private isFull (digest: Digest) : bool =
        digest.M0 = UInt64.MaxValue && digest.M1 = UInt64.MaxValue && digest.M2 = UInt64.MaxValue

    /// <summary>Adds every glyph of a class's ranges to a digest, <c>ClassDef::collect_class</c>.</summary>
    /// <param name="ranges">The class's sorted disjoint ranges.</param>
    /// <param name="index">The next range.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private addRanges (ranges: ReadOnlySpan<CodepointRange>) (index: int) (digest: Digest) : Digest =
        match index < ranges.Length with
        | false -> digest
        | true -> addRanges ranges (index + 1) (Digest.addRange ranges[index].First ranges[index].Last digest)

    /// <summary>Collects a glyph or a class value into a digest, <c>Rule::collect_second_glyphs</c>'s tail.</summary>
    /// <param name="digest">The digest.</param>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="second">The second glyph or class value.</param>
    /// <returns>The digest, full when a class rule names class zero.</returns>
    let private addSecond (digest: Digest) (classDef: ClassDef voption) (second: uint32) : Digest =
        match classDef with
        | ValueNone -> Digest.add second digest
        | ValueSome classDef ->
            match second with
            | 0u -> Digest.full
            | _ ->
                let ranges = ClassDef.glyphsInClass classDef second
                addRanges (ranges.AsSpan()) 0 digest

    /// <summary>The second-glyph digest of one ligature set, <c>LigatureSet::collect_seconds</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="set">The ligature-set offset.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest, full when a ligature has no components.</returns>
    [<TailCall>]
    let rec private ligatureSetSeconds (table: ReadOnlySpan<byte>) (set: int) (w: int) (index: int) (digest: Digest) : Digest =
        match index < int (Binary.u16 table set) with
        | false -> digest
        | true ->
            let next =
                match follow set (Coverage.readWide table (set + 2 + index * w) w) with
                | ValueNone -> Digest.full
                | ValueSome lig ->
                    let compCount = int (Binary.u16 table (lig + w))
                    match compCount <= 1 with
                    | true -> Digest.full
                    | false -> Digest.add (Coverage.readWide table (lig + w + 2) w) digest
            match isFull next with
            | true -> Digest.full
            | false -> ligatureSetSeconds table set w (index + 1) next

    /// <summary>The second-glyph digest of a ligature subtable, <c>LigatureSubstFormat1::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="index">The next ligature set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private ligatureSeconds (table: ReadOnlySpan<byte>) (o: int) (w: int) (index: int) (digest: Digest) : Digest =
        let setCount = int (Coverage.readWide table (o + 2 + w) w)
        match index < setCount with
        | false -> digest
        | true ->
            let next =
                match follow o (Coverage.readWide table (o + 2 + 2 * w + index * w) w) with
                | ValueNone -> digest
                | ValueSome set -> ligatureSetSeconds table set w 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> ligatureSeconds table o w (index + 1) next

    /// <summary>The second-glyph digest of one context rule, <c>Rule::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The input value width.</param>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest, full when the rule has no second glyph.</returns>
    let private contextRuleSecond (table: ReadOnlySpan<byte>) (rule: int) (vw: int) (classDef: ClassDef voption) (digest: Digest) : Digest =
        let inputCount = int (Binary.u16 table rule)
        match inputCount <= 1 with
        | true -> Digest.full
        | false -> addSecond digest classDef (Coverage.readWide table (rule + 4) vw)

    /// <summary>The second-glyph digest of one context rule set, <c>RuleSet::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="set">The rule-set offset.</param>
    /// <param name="rw">The rule-offset width.</param>
    /// <param name="vw">The input value width.</param>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest, full when a rule has no second glyph.</returns>
    [<TailCall>]
    let rec private ruleSetSeconds
            (table: ReadOnlySpan<byte>) (set: int) (rw: int) (vw: int) (classDef: ClassDef voption) (index: int) (digest: Digest) : Digest =
        match index < int (Binary.u16 table set) with
        | false -> digest
        | true ->
            let next =
                match follow set (Coverage.readWide table (set + 2 + index * rw) rw) with
                | ValueNone -> Digest.full
                | ValueSome rule -> contextRuleSecond table rule vw classDef digest
            match isFull next with
            | true -> Digest.full
            | false -> ruleSetSeconds table set rw vw classDef (index + 1) next

    /// <summary>The second-glyph digest of a context subtable of format 1, 2, 4 or 5.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="index">The next rule set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private contextSetSeconds (table: ReadOnlySpan<byte>) (o: int) (format: int) (w: int) (index: int) (digest: Digest) : Digest =
        let classRules = format = 2 || format = 5
        let vw = match classRules with | true -> Width16 | false -> w
        let rw = match classRules with | true -> w | false -> Width16
        let classDef =
            match classRules with
            | true -> ValueSome (classDefAt table o (Coverage.readWide table (o + 2 + w) w))
            | false -> ValueNone
        let setCount = int (Coverage.readWide table (o + 2 + (match classRules with | true -> 2 | false -> 1) * w) w)
        let setStart = o + 2 + (match classRules with | true -> 3 | false -> 2) * w
        match index < setCount with
        | false -> digest
        | true ->
            let next =
                match follow o (Coverage.readWide table (setStart + index * w) w) with
                | ValueNone -> digest
                | ValueSome set -> ruleSetSeconds table set rw vw classDef 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> contextSetSeconds table o format w (index + 1) next

    /// <summary>The second-glyph digest of a context subtable of format 3 or 6, <c>ContextFormat3_6::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The offset width.</param>
    /// <returns>The digest.</returns>
    let private context3Seconds (table: ReadOnlySpan<byte>) (o: int) (w: int) : Digest =
        let glyphCount = int (Binary.u16 table (o + 2))
        match glyphCount <= 1 with
        | true -> Digest.full
        | false -> covAt table o (Coverage.readWide table (o + 6 + w) w)

    /// <summary>The second-glyph digest of a context subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="w">The offset width.</param>
    /// <returns>The digest.</returns>
    let private contextSeconds (table: ReadOnlySpan<byte>) (o: int) (format: int) (w: int) : Digest =
        match format = 3 || format = 6 with
        | true -> context3Seconds table o w
        | false -> contextSetSeconds table o format w 0 Digest.empty

    /// <summary>The second-glyph digest of one chain rule, <c>ChainRule::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The input value width.</param>
    /// <param name="inputClassDef">The input class definition, none for glyph rules.</param>
    /// <param name="lookaheadClassDef">The lookahead class definition, none for glyph rules.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest, full when the rule has no second glyph.</returns>
    let private chainRuleSecond
            (table: ReadOnlySpan<byte>) (rule: int) (vw: int) (inputClassDef: ClassDef voption) (lookaheadClassDef: ClassDef voption) (digest: Digest) : Digest =
        let backtrackCount = int (Binary.u16 table rule)
        let inputCount = int (Binary.u16 table (rule + 2 + backtrackCount * vw))
        match inputCount > 1 with
        | true -> addSecond digest inputClassDef (Coverage.readWide table (rule + 4 + backtrackCount * vw) vw)
        | false ->
            let lookaheadCount = int (Binary.u16 table (rule + 4 + backtrackCount * vw + (inputCount - 1) * vw))
            match lookaheadCount > 0 with
            | true -> addSecond digest lookaheadClassDef (Coverage.readWide table (rule + 6 + backtrackCount * vw + (inputCount - 1) * vw) vw)
            | false -> Digest.full

    /// <summary>The second-glyph digest of one chain rule set, <c>ChainRuleSet::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="set">The rule-set offset.</param>
    /// <param name="rw">The rule-offset width.</param>
    /// <param name="vw">The input value width.</param>
    /// <param name="inputClassDef">The input class definition, none for glyph rules.</param>
    /// <param name="lookaheadClassDef">The lookahead class definition, none for glyph rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest, full when a rule has no second glyph.</returns>
    [<TailCall>]
    let rec private chainRuleSetSeconds
            (table: ReadOnlySpan<byte>) (set: int) (rw: int) (vw: int) (inputClassDef: ClassDef voption) (lookaheadClassDef: ClassDef voption) (index: int) (digest: Digest) : Digest =
        match index < int (Binary.u16 table set) with
        | false -> digest
        | true ->
            let next =
                match follow set (Coverage.readWide table (set + 2 + index * rw) rw) with
                | ValueNone -> Digest.full
                | ValueSome rule -> chainRuleSecond table rule vw inputClassDef lookaheadClassDef digest
            match isFull next with
            | true -> Digest.full
            | false -> chainRuleSetSeconds table set rw vw inputClassDef lookaheadClassDef (index + 1) next

    /// <summary>The second-glyph digest of a chain subtable of format 1, 2, 4 or 5.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="index">The next rule set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private chainSetSeconds (table: ReadOnlySpan<byte>) (o: int) (format: int) (w: int) (index: int) (digest: Digest) : Digest =
        let classRules = format = 2 || format = 5
        let vw = match classRules with | true -> Width16 | false -> w
        let struct (inputClassDef, lookaheadClassDef) =
            match classRules with
            | true ->
                struct (ValueSome (classDefAt table o (Coverage.readWide table (o + 2 + 2 * w) w)),
                        ValueSome (classDefAt table o (Coverage.readWide table (o + 2 + 3 * w) w)))
            | false -> struct (ValueNone, ValueNone)
        let setCount =
            match classRules with
            | true -> int (Binary.u16 table (o + 2 + 4 * w))
            | false -> int (Coverage.readWide table (o + 2 + w) w)
        let setStart =
            match classRules with
            | true -> o + 4 + 4 * w
            | false -> o + 2 + 2 * w
        match index < setCount with
        | false -> digest
        | true ->
            let next =
                match follow o (Coverage.readWide table (setStart + index * w) w) with
                | ValueNone -> digest
                | ValueSome set -> chainRuleSetSeconds table set w vw inputClassDef lookaheadClassDef 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> chainSetSeconds table o format w (index + 1) next

    /// <summary>The second-glyph digest of a chain subtable of format 3, <c>ChainContextFormat3::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The offset width.</param>
    /// <returns>The digest.</returns>
    let private chain3Seconds (table: ReadOnlySpan<byte>) (o: int) (w: int) : Digest =
        let backtrackCount = int (Binary.u16 table (o + 2))
        let inputCount = int (Binary.u16 table (o + 4 + backtrackCount * w))
        match inputCount > 1 with
        | true -> covAt table o (Coverage.readWide table (o + 6 + backtrackCount * w + w) w)
        | false ->
            let lookaheadCount = int (Binary.u16 table (o + 6 + backtrackCount * w + inputCount * w))
            match lookaheadCount > 0 with
            | true -> covAt table o (Coverage.readWide table (o + 8 + backtrackCount * w + inputCount * w) w)
            | false -> Digest.full

    /// <summary>The second-glyph digest of a chain subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="w">The offset width.</param>
    /// <returns>The digest.</returns>
    let private chainSeconds (table: ReadOnlySpan<byte>) (o: int) (format: int) (w: int) : Digest =
        match format = 3 with
        | true -> chain3Seconds table o w
        | false -> chainSetSeconds table o format w 0 Digest.empty

    /// <summary>The byte count of a value record of a value format, <c>ValueFormat::get_size</c>.</summary>
    /// <param name="format">The value format.</param>
    /// <param name="acc">The bits counted so far.</param>
    /// <returns>Two bytes per set value or device field.</returns>
    [<TailCall>]
    let rec private valueSizeBits (format: int) (acc: int) : int =
        match format = 0 with
        | true -> acc
        | false -> valueSizeBits (format &&& (format - 1)) (acc + 1)

    /// <summary>The byte count of a value record of a value format, <c>ValueFormat::get_size</c>.</summary>
    /// <param name="format">The value format.</param>
    /// <returns>Two bytes per set value or device field.</returns>
    let private valueSize (format: int) : int =
        valueSizeBits (format &&& 0xFF) 0 * 2

    /// <summary>The second-glyph digest of one pair set, <c>PairSet::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="set">The pair-set offset.</param>
    /// <param name="vw">The value width.</param>
    /// <param name="recordSize">The pair-value-record size.</param>
    /// <param name="index">The next record.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private pairSetSeconds (table: ReadOnlySpan<byte>) (set: int) (vw: int) (recordSize: int) (index: int) (digest: Digest) : Digest =
        match index < int (Coverage.readWide table set vw) with
        | false -> digest
        | true ->
            let second = Coverage.readWide table (set + vw + index * recordSize) vw
            pairSetSeconds table set vw recordSize (index + 1) (Digest.add second digest)

    /// <summary>The second-glyph digest of a pair subtable of format 1 or 3, <c>PairPosFormat1::collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="index">The next pair set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private pairSeconds (table: ReadOnlySpan<byte>) (o: int) (w: int) (index: int) (digest: Digest) : Digest =
        let valueFormat1 = int (Binary.u16 table (o + 2 + w))
        let valueFormat2 = int (Binary.u16 table (o + 4 + w))
        let recordSize = w + valueSize valueFormat1 + valueSize valueFormat2
        let setCount = int (Coverage.readWide table (o + 6 + w) w)
        match index < setCount with
        | false -> digest
        | true ->
            let next =
                match follow o (Coverage.readWide table (o + 8 + w + index * w) w) with
                | ValueNone -> digest
                | ValueSome set -> pairSetSeconds table set w recordSize 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> pairSeconds table o w (index + 1) next

    /// <summary>The second-glyph digest of a mark-attachment subtable: the base, ligature or mark2 coverage.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage offsets, <c>LOffsetTo</c>: 32 bits in the medium formats.</param>
    /// <returns>The digest.</returns>
    let private markSeconds (table: ReadOnlySpan<byte>) o covW =
        covAt table o (readCoverageOffset table (o + 2 + covW) covW)

    /// <summary>The second-glyph digest of a subtable, <c>collect_second_glyphs</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The digest, full when the subtable collects no second glyphs.</returns>
    [<TailCall>]
    let rec private secondDigest (table: ReadOnlySpan<byte>) (o: int) (lookupType: int) (isGsub: bool) : Digest =
        match isExtension lookupType isGsub with
        | true ->
            match follow o (Binary.u32 table (o + 4)) with
            | ValueNone -> Digest.full
            | ValueSome inner -> secondDigest table inner (int (Binary.u16 table (o + 2))) isGsub
        | false ->
            let format = int (Binary.u16 table o)
            let w = offsetWidth lookupType format isGsub
            match isGsub, lookupType with
            | true, 4 -> ligatureSeconds table o w 0 Digest.empty
            | true, 5 -> contextSeconds table o format w
            | true, 6 -> chainSeconds table o format w
            | false, 2 when format = 1 || format = 3 -> pairSeconds table o w 0 Digest.empty
            | false, 3 -> coverageDigest table o lookupType isGsub
            | false, 4 | false, 5 | false, 6 -> markSeconds table o (coverageWidth lookupType format isGsub)
            | false, 7 -> contextSeconds table o format w
            | false, 8 -> chainSeconds table o format w
            | _ -> Digest.full

    /// <summary>Collects a subtable's coverage digest and second-glyph digest from raw bytes.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>The coverage digest and the second-glyph digest.</returns>
    let collect (table: ReadOnlySpan<byte>) (o: int) (lookupType: int) (isGsub: bool) : struct (Digest * Digest) =
        struct (coverageDigest table o lookupType isGsub, secondDigest table o lookupType isGsub)
