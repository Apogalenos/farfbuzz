namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>One ligature of a ligature set: the replacement glyph and the components after the first,
/// <c>Ligature</c>. A default <see cref="Components"/> marks the Null ligature (no components at all).</summary>
[<Struct>]
type internal Ligature =
    { LigGlyph: uint32
      Components: ImmutableArray<uint32> }

/// <summary>A GSUB subtable. Single, multiple, alternate and ligature subtables carry their decoded coverage and
/// glyph arrays; the remaining types stay <c>SubstPending</c> with their precomputed digests until S09.</summary>
type internal SubstSubtable =
    | SingleDelta of singleCoverage: Coverage * delta: uint32 * glyphMask: uint32
    | SingleList of listCoverage: Coverage * substitutes: ImmutableArray<uint32>
    | Multiple of multipleCoverage: Coverage * sequences: ImmutableArray<ImmutableArray<uint32>>
    | Alternate of alternateCoverage: Coverage * alternateSets: ImmutableArray<ImmutableArray<uint32>>
    | LigatureSubst of ligatureCoverage: Coverage * ligatureSets: ImmutableArray<ImmutableArray<Ligature>> * secondDigest: Digest
    | SubstContext of substRules: ContextRules
    | SubstChainContext of substChainRules: ChainRules
    | ReverseChainSingle of reverseCoverage: Coverage * reverseBacktrack: ImmutableArray<Coverage> * reverseLookahead: ImmutableArray<Coverage> * reverseSubstitutes: ImmutableArray<uint32>
    | SubstPending of substType: int * substFormat: int * coverageDigest: Digest * second: Digest

/// <summary>The GSUB subtable parser and digest accessors, <c>SubstLookupSubTable</c>.</summary>
[<RequireQualifiedAccess>]
module internal GsubTables =
    /// <summary>The single-substitution lookup type, <c>SubstLookupSubTable::Single</c>.</summary>
    [<Literal>]
    let private SingleType = 1

    /// <summary>The multiple-substitution lookup type, <c>SubstLookupSubTable::Multiple</c>.</summary>
    [<Literal>]
    let private MultipleType = 2

    /// <summary>The alternate-substitution lookup type, <c>SubstLookupSubTable::Alternate</c>.</summary>
    [<Literal>]
    let private AlternateType = 3

    /// <summary>The ligature-substitution lookup type, <c>SubstLookupSubTable::Ligature</c>.</summary>
    [<Literal>]
    let private LigatureType = 4

    /// <summary>The extension lookup type, <c>SubstLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private ExtensionType = 7

    /// <summary>The context lookup type, <c>SubstLookupSubTable::Context</c>.</summary>
    [<Literal>]
    let private ContextType = 5

    /// <summary>The chained-context lookup type, <c>SubstLookupSubTable::ChainContext</c>.</summary>
    [<Literal>]
    let private ChainType = 6

    /// <summary>The reverse-chaining single lookup type, <c>SubstLookupSubTable::ReverseChainSingle</c>.</summary>
    [<Literal>]
    let private ReverseType = 8

    /// <summary>The byte width of a 16-bit offset or glyph id.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The byte width of a 24-bit offset or glyph id.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>The 16-bit single-substitution glyph mask, <c>SmallTypes</c>.</summary>
    [<Literal>]
    let private Mask16 = 0xFFFFu

    /// <summary>The 24-bit single-substitution glyph mask, <c>MediumTypes</c>.</summary>
    [<Literal>]
    let private Mask24 = 0xFFFFFFu

    /// <summary>Reads a coverage reached through a relative offset field, Null for a zero offset, with its direct table when it is worth one.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The relative coverage offset.</param>
    /// <returns>The coverage.</returns>
    let private coverageAt (table: ReadOnlySpan<byte>) o offsetValue =
        match offsetValue with
        | 0u -> CoverageNull
        | _ -> Coverage.direct (Coverage.parse table (o + int offsetValue))

    /// <summary>Reads a coverage offset of one width, <c>LOffsetTo</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <param name="width">Two or four bytes.</param>
    /// <returns>The offset value.</returns>
    let private coverageOffset (table: ReadOnlySpan<byte>) offset width =
        match width with
        | 4 -> Binary.u32 table offset
        | _ -> uint32 (Binary.u16 table offset)

    /// <summary>Decodes a glyph array of one width.</summary>
    [<Struct>]
    type private GlyphArrayDecoder =
        { Base: int
          Width: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (table, index) =
                struct (Coverage.readWide table (this.Base + index * this.Width) this.Width, index + 1)

    /// <summary>Decodes a glyph array, <c>ArrayOf&lt;HBGlyphID&gt;</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="baseOffset">The first glyph offset.</param>
    /// <param name="count">The glyph count.</param>
    /// <param name="width">The glyph width.</param>
    /// <returns>The glyphs.</returns>
    let private glyphArray (table: ReadOnlySpan<byte>) baseOffset count width =
        Sealed.unfoldSpan table count 0 { GlyphArrayDecoder.Base = baseOffset; Width = width }

    /// <summary>Decodes the sequences of a multiple-substitution subtable.</summary>
    [<Struct>]
    type private SequenceDecoder =
        { Subtable: int
          CoverageWidth: int
          Width: int }
        interface ISpanUnfolder<byte, int, ImmutableArray<uint32>> with
            member this.Next (table, index) =
                let offsetsBase = this.Subtable + 2 + this.CoverageWidth + this.Width
                let offset = Coverage.readWide table (offsetsBase + index * this.Width) this.Width
                match offset with
                | 0u -> struct (ImmutableArray.Empty, index + 1)
                | _ ->
                    let at = this.Subtable + int offset
                    struct (glyphArray table (at + 2) (int (Binary.u16 table at)) this.Width, index + 1)

    /// <summary>Decodes the alternate sets of an alternate-substitution subtable.</summary>
    [<Struct>]
    type private AlternateSetDecoder =
        { Subtable: int
          CoverageWidth: int
          Width: int }
        interface ISpanUnfolder<byte, int, ImmutableArray<uint32>> with
            member this.Next (table, index) =
                let offsetsBase = this.Subtable + 2 + this.CoverageWidth + this.Width
                let offset = Coverage.readWide table (offsetsBase + index * this.Width) this.Width
                match offset with
                | 0u -> struct (ImmutableArray.Empty, index + 1)
                | _ ->
                    let at = this.Subtable + int offset
                    struct (glyphArray table (at + 2) (int (Binary.u16 table at)) this.Width, index + 1)

    /// <summary>Parses a single-substitution subtable, <c>SingleSubst</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseSingle (table: ReadOnlySpan<byte>) o =
        match Binary.u16 table o with
        | 1us ->
            let coverage = coverageAt table o (uint32 (Binary.u16 table (o + 2)))
            let delta = uint32 (Binary.u16 table (o + 4))
            SingleDelta (coverage, delta, Mask16)
        | 3us ->
            let coverage = coverageAt table o (Binary.u32 table (o + 2))
            let delta = Binary.u24 table (o + 6)
            SingleDelta (coverage, delta, Mask24)
        | 2us ->
            let coverage = coverageAt table o (uint32 (Binary.u16 table (o + 2)))
            let count = int (Binary.u16 table (o + 4))
            SingleList (coverage, glyphArray table (o + 6) count Width16)
        | 4us ->
            let coverage = coverageAt table o (Binary.u32 table (o + 2))
            let count = int (Binary.u24 table (o + 6))
            SingleList (coverage, glyphArray table (o + 9) count Width24)
        | format -> SubstPending (SingleType, int format, Digest.empty, Digest.full)

    /// <summary>Parses a multiple-substitution subtable, <c>MultipleSubst</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseMultiple (table: ReadOnlySpan<byte>) o =
        match Binary.u16 table o with
        | 1us ->
            let coverage = coverageAt table o (uint32 (Binary.u16 table (o + 2)))
            let count = int (Binary.u16 table (o + 4))
            Multiple (coverage, Sealed.unfoldSpan table count 0 { SequenceDecoder.Subtable = o; CoverageWidth = 2; Width = Width16 })
        | 2us ->
            let coverage = coverageAt table o (Binary.u32 table (o + 2))
            let count = int (Binary.u24 table (o + 6))
            Multiple (coverage, Sealed.unfoldSpan table count 0 { SequenceDecoder.Subtable = o; CoverageWidth = 4; Width = Width24 })
        | format -> SubstPending (MultipleType, int format, Digest.empty, Digest.full)

    /// <summary>Parses an alternate-substitution subtable, <c>AlternateSubst</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseAlternate (table: ReadOnlySpan<byte>) o =
        match Binary.u16 table o with
        | 1us ->
            let coverage = coverageAt table o (uint32 (Binary.u16 table (o + 2)))
            let count = int (Binary.u16 table (o + 4))
            Alternate (coverage, Sealed.unfoldSpan table count 0 { AlternateSetDecoder.Subtable = o; CoverageWidth = 2; Width = Width16 })
        | 2us ->
            let coverage = coverageAt table o (Binary.u32 table (o + 2))
            let count = int (Binary.u24 table (o + 6))
            Alternate (coverage, Sealed.unfoldSpan table count 0 { AlternateSetDecoder.Subtable = o; CoverageWidth = 4; Width = Width24 })
        | format -> SubstPending (AlternateType, int format, Digest.empty, Digest.full)

    /// <summary>Folds one ligature set's second glyphs into a digest, full when a ligature has no components,
    /// <c>Ligature::collect_second</c>.</summary>
    /// <param name="ligatures">The ligatures of one set.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private foldLigatureSetSeconds (ligatures: ReadOnlySpan<Ligature>) (index: int) (digest: Digest) =
        match index < ligatures.Length with
        | false -> digest
        | true ->
            let next =
                match ligatures[index].Components.IsDefault || ligatures[index].Components.Length = 0 with
                | true -> Digest.full
                | false -> Digest.add ligatures[index].Components[0] digest
            match next.M0 = UInt64.MaxValue && next.M1 = UInt64.MaxValue && next.M2 = UInt64.MaxValue with
            | true -> Digest.full
            | false -> foldLigatureSetSeconds ligatures (index + 1) next

    /// <summary>Folds every ligature set's second glyphs into the subtable digest,
    /// <c>LigatureSubstFormat1::collect_second_glyphs</c>.</summary>
    /// <param name="sets">The ligature sets.</param>
    /// <param name="index">The next set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private foldLigatureSetsSeconds (sets: ReadOnlySpan<ImmutableArray<Ligature>>) (index: int) (digest: Digest) =
        match index < sets.Length with
        | false -> digest
        | true ->
            let next = foldLigatureSetSeconds (sets[index].AsSpan()) 0 digest
            match next.M0 = UInt64.MaxValue && next.M1 = UInt64.MaxValue && next.M2 = UInt64.MaxValue with
            | true -> Digest.full
            | false -> foldLigatureSetsSeconds sets (index + 1) next

    /// <summary>The second-glyph digest of a parsed ligature subtable, full when any ligature has no components.</summary>
    /// <param name="sets">The ligature sets.</param>
    /// <returns>The digest.</returns>
    let private ligatureSecondDigest (sets: ImmutableArray<ImmutableArray<Ligature>>) =
        foldLigatureSetsSeconds (sets.AsSpan()) 0 Digest.empty

    /// <summary>Decodes the component glyphs of one ligature, <c>HeadlessArray16Of&lt;HBGlyphID&gt;</c>.</summary>
    [<Struct>]
    type private LigatureComponentsDecoder =
        { Base: int
          Width: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (table, index) =
                struct (Coverage.readWide table (this.Base + index * this.Width) this.Width, index + 1)

    /// <summary>Parses one ligature reached through a non-zero offset, <c>Ligature</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="lig">The ligature offset.</param>
    /// <param name="width">The glyph width.</param>
    /// <returns>The ligature, the Null ligature for a zero component count.</returns>
    let private parseLigature (table: ReadOnlySpan<byte>) lig width =
        let ligGlyph = Coverage.readWide table lig width
        let componentCount = int (Binary.u16 table (lig + width))
        match componentCount with
        | 0 -> { LigGlyph = 0u; Components = Unchecked.defaultof<ImmutableArray<uint32>> }
        | _ ->
            { LigGlyph = ligGlyph
              Components = Sealed.unfoldSpan table (componentCount - 1) 0 { LigatureComponentsDecoder.Base = lig + width + 2; Width = width } }

    /// <summary>Decodes the ligatures of one ligature set, <c>LigatureSet</c>.</summary>
    [<Struct>]
    type private LigatureDecoder =
        { SetOffset: int
          Width: int }
        interface ISpanUnfolder<byte, int, Ligature> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.SetOffset + 2 + index * this.Width) this.Width
                match offset with
                | 0u -> struct ({ LigGlyph = 0u; Components = Unchecked.defaultof<ImmutableArray<uint32>> }, index + 1)
                | _ -> struct (parseLigature table (this.SetOffset + int offset) this.Width, index + 1)

    /// <summary>Decodes the ligature sets of a ligature subtable, <c>LigatureSubstFormat1_2</c>.</summary>
    [<Struct>]
    type private LigatureSetDecoder =
        { Subtable: int
          CoverageWidth: int
          Width: int }
        interface ISpanUnfolder<byte, int, ImmutableArray<Ligature>> with
            member this.Next (table, index) =
                let offsetsBase = this.Subtable + 2 + this.CoverageWidth + this.Width
                let offset = Coverage.readWide table (offsetsBase + index * this.Width) this.Width
                match offset with
                | 0u -> struct (ImmutableArray.Empty, index + 1)
                | _ ->
                    let at = this.Subtable + int offset
                    struct (Sealed.unfoldSpan table (int (Binary.u16 table at)) 0 { LigatureDecoder.SetOffset = at; Width = this.Width }, index + 1)

    /// <summary>Parses a ligature-substitution subtable, <c>LigatureSubst</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseLigatureSubst (table: ReadOnlySpan<byte>) o =
        match Binary.u16 table o with
        | 1us ->
            let coverage = coverageAt table o (uint32 (Binary.u16 table (o + 2)))
            let count = int (Binary.u16 table (o + 4))
            let sets = Sealed.unfoldSpan table count 0 { LigatureSetDecoder.Subtable = o; CoverageWidth = 2; Width = Width16 }
            LigatureSubst (coverage, sets, ligatureSecondDigest sets)
        | 2us ->
            let coverage = coverageAt table o (Binary.u32 table (o + 2))
            let count = int (Binary.u24 table (o + 6))
            let sets = Sealed.unfoldSpan table count 0 { LigatureSetDecoder.Subtable = o; CoverageWidth = 4; Width = Width24 }
            LigatureSubst (coverage, sets, ligatureSecondDigest sets)
        | format -> SubstPending (LigatureType, int format, Digest.empty, Digest.full)

    /// <summary>The coverage digest of a context subtable, <c>get_coverage ().collect_coverage</c>: the coverage of the first
    /// glyph, which for the third format is the first input coverage.</summary>
    /// <param name="rules">The context rules.</param>
    /// <returns>The digest.</returns>
    let contextDigest (rules: ContextRules) =
        match rules with
        | GlyphRules (coverage, _)
        | ClassRules (coverage, _, _, _) -> Coverage.digest coverage
        | CoverageRule (coverages, _) ->
            match coverages.IsEmpty with
            | true -> Digest.empty
            | false -> Coverage.digest coverages[0]

    /// <summary>The coverage digest of a chained-context subtable, <c>get_coverage ().collect_coverage</c>.</summary>
    /// <param name="rules">The chained-context rules.</param>
    /// <returns>The digest.</returns>
    let chainDigest (rules: ChainRules) =
        match rules with
        | ChainGlyphRules (coverage, _)
        | ChainClassRules (coverage, _, _, _, _, _) -> Coverage.digest coverage
        | ChainCoverageRule (_, input, _, _) ->
            match input.IsEmpty with
            | true -> Digest.empty
            | false -> Coverage.digest input[0]

    /// <summary>The coverage digest of a parsed GSUB subtable, <c>get_coverage ().collect_coverage</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The digest.</returns>
    let subtableDigest subtable =
        match subtable with
        | SingleDelta (coverage, _, _)
        | SingleList (coverage, _)
        | Multiple (coverage, _)
        | Alternate (coverage, _)
        | LigatureSubst (coverage, _, _) -> Coverage.digest coverage
        | SubstContext rules -> contextDigest rules
        | SubstChainContext rules -> chainDigest rules
        | ReverseChainSingle (coverage, _, _, _) -> Coverage.digest coverage
        | SubstPending (_, _, coverage, _) -> coverage

    /// <summary>Whether a digest is full, <c>hb_set_digest_t::full</c>.</summary>
    /// <param name="digest">The digest.</param>
    /// <returns>True when every mask is all bits set.</returns>
    let private isFull digest =
        digest.M0 = UInt64.MaxValue && digest.M1 = UInt64.MaxValue && digest.M2 = UInt64.MaxValue

    /// <summary>Adds every glyph of a class's ranges to a digest, <c>ClassDef::collect_class</c>.</summary>
    /// <param name="ranges">The class's sorted disjoint ranges.</param>
    /// <param name="index">The next range.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private addRanges (ranges: ReadOnlySpan<CodepointRange>) index digest =
        match index < ranges.Length with
        | false -> digest
        | true -> addRanges ranges (index + 1) (Digest.addRange ranges[index].First ranges[index].Last digest)

    /// <summary>Adds a second glyph or class to a digest, <c>Rule::collect_second_glyphs</c>'s tail.</summary>
    /// <param name="digest">The digest.</param>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="second">The second glyph or class value.</param>
    /// <returns>The digest, full when a class rule names class zero.</returns>
    let private addSecond digest classDef second =
        match classDef with
        | ValueNone -> Digest.add second digest
        | ValueSome classDef ->
            match second with
            | 0u -> Digest.full
            | _ -> addRanges ((ClassDef.glyphsInClass classDef second).AsSpan()) 0 digest

    /// <summary>The second-glyph digest of one context rule, <c>Rule::collect_second_glyphs</c>.</summary>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="digest">The digest so far.</param>
    /// <param name="input">The input values after the first glyph.</param>
    /// <returns>The digest, full when the rule has no second glyph.</returns>
    let private contextRuleSecond (classDef: ClassDef voption) digest (input: ImmutableArray<uint32>) =
        match input.IsEmpty with
        | true -> Digest.full
        | false -> addSecond digest classDef input[0]

    /// <summary>The second-glyph digest of one context rule set.</summary>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private contextSetSecond (classDef: ClassDef voption) (rules: ImmutableArray<Rule>) index digest =
        match index < rules.Length with
        | false -> digest
        | true ->
            let next = contextRuleSecond classDef digest rules[index].Input
            match isFull next with
            | true -> Digest.full
            | false -> contextSetSecond classDef rules (index + 1) next

    /// <summary>The second-glyph digest of one chain rule, <c>ChainRule::collect_second_glyphs</c>.</summary>
    /// <param name="inputClassDef">The input class definition, none for glyph rules.</param>
    /// <param name="lookaheadClassDef">The lookahead class definition, none for glyph rules.</param>
    /// <param name="rule">The rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    let private chainRuleSecond inputClassDef lookaheadClassDef rule digest =
        match rule.ChainInput.IsEmpty with
        | false -> addSecond digest inputClassDef rule.ChainInput[0]
        | true ->
            match rule.Lookahead.IsEmpty with
            | false -> addSecond digest lookaheadClassDef rule.Lookahead[0]
            | true -> Digest.full

    /// <summary>The second-glyph digest of one chain rule set.</summary>
    /// <param name="inputClassDef">The input class definition.</param>
    /// <param name="lookaheadClassDef">The lookahead class definition.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private chainSetSecond (inputClassDef: ClassDef voption) (lookaheadClassDef: ClassDef voption) (rules: ImmutableArray<ChainRule>) index digest =
        match index < rules.Length with
        | false -> digest
        | true ->
            let next = chainRuleSecond inputClassDef lookaheadClassDef rules[index] digest
            match isFull next with
            | true -> Digest.full
            | false -> chainSetSecond inputClassDef lookaheadClassDef rules (index + 1) next

    /// <summary>Folds the context rule sets' second glyphs into a digest.</summary>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="index">The next set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private foldRuleSets (classDef: ClassDef voption) (ruleSets: ImmutableArray<ImmutableArray<Rule>>) index digest =
        match index < ruleSets.Length with
        | false -> digest
        | true ->
            let next = contextSetSecond classDef ruleSets[index] 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> foldRuleSets classDef ruleSets (index + 1) next

    /// <summary>The second-glyph digest of one context subtable, <c>Context::collect_second_glyphs</c>.</summary>
    /// <param name="rules">The context rules.</param>
    /// <returns>The digest.</returns>
    let contextSecondDigest (rules: ContextRules) =
        match rules with
        | GlyphRules (_, ruleSets) -> foldRuleSets ValueNone ruleSets 0 Digest.empty
        | ClassRules (_, classDef, ruleSets, _) -> foldRuleSets (ValueSome classDef) ruleSets 0 Digest.empty
        | CoverageRule (coverages, _) ->
            match coverages.Length <= 1 with
            | true -> Digest.full
            | false -> Coverage.digest coverages[1]

    /// <summary>Folds the chain rule sets' second glyphs into a digest.</summary>
    /// <param name="inputClassDef">The input class definition.</param>
    /// <param name="lookaheadClassDef">The lookahead class definition.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="index">The next set.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private foldChainSets (inputClassDef: ClassDef voption) (lookaheadClassDef: ClassDef voption) (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) index digest =
        match index < ruleSets.Length with
        | false -> digest
        | true ->
            let next = chainSetSecond inputClassDef lookaheadClassDef ruleSets[index] 0 digest
            match isFull next with
            | true -> Digest.full
            | false -> foldChainSets inputClassDef lookaheadClassDef ruleSets (index + 1) next

    /// <summary>The second-glyph digest of one chained-context subtable.</summary>
    /// <param name="rules">The chain rules.</param>
    /// <returns>The digest.</returns>
    let chainSecondDigest (rules: ChainRules) =
        match rules with
        | ChainGlyphRules (_, ruleSets) -> foldChainSets ValueNone ValueNone ruleSets 0 Digest.empty
        | ChainClassRules (_, _, inputClassDef, lookaheadClassDef, ruleSets, _) -> foldChainSets (ValueSome inputClassDef) (ValueSome lookaheadClassDef) ruleSets 0 Digest.empty
        | ChainCoverageRule (_, input, lookahead, _) ->
            match input.Length > 1 with
            | true -> Coverage.digest input[1]
            | false ->
                match lookahead.IsEmpty with
                | false -> Coverage.digest lookahead[0]
                | true -> Digest.full

    /// <summary>The second-glyph digest of a parsed GSUB subtable, <c>collect_second_glyphs</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The digest, full when the subtable collects no second glyph.</returns>
    let secondDigest subtable =
        match subtable with
        | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ -> Digest.full
        | LigatureSubst (_, _, second) -> second
        | SubstContext rules -> contextSecondDigest rules
        | SubstChainContext rules -> chainSecondDigest rules
        | ReverseChainSingle _ -> Digest.full
        | SubstPending (_, _, _, second) -> second

    /// <summary>The component count of a ligature, including the first glyph; the Null ligature has none.</summary>
    /// <param name="lig">The ligature.</param>
    /// <returns>The count.</returns>
    let ligCount (lig: Ligature) =
        match lig.Components.IsDefault with
        | true -> 0
        | false -> lig.Components.Length + 1

    /// <summary>Reads a coverage reached through a relative offset field, Null for a zero offset, with its direct table when it is worth one.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="baseOffset">The offset the coverage offset is relative to.</param>
    /// <param name="offsetValue">The relative coverage offset.</param>
    /// <returns>The coverage.</returns>
    let private coverageReached (table: ReadOnlySpan<byte>) baseOffset offsetValue =
        match offsetValue with
        | 0u -> CoverageNull
        | _ -> Coverage.direct (Coverage.parse table (baseOffset + int offsetValue))

    /// <summary>Reads a class definition reached through a relative offset field, Null for a zero offset, with its direct table when it is worth one.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="baseOffset">The offset the class-definition offset is relative to.</param>
    /// <param name="offsetValue">The relative class-definition offset.</param>
    /// <returns>The class definition.</returns>
    let private classDefReached (table: ReadOnlySpan<byte>) baseOffset offsetValue =
        match offsetValue with
        | 0u -> ClassDefNull
        | _ -> ClassDef.direct (ClassDef.parse table (baseOffset + int offsetValue))

    /// <summary>Unfolds a lookup-record array.</summary>
    [<Struct>]
    type private LookupRecordDecoder =
        { Base: int }
        interface ISpanUnfolder<byte, int, LookupRecord> with
            member this.Next (table, index) =
                let at = this.Base + index * 4
                struct ({ SequenceIndex = int (Binary.u16 table at); LookupListIndex = int (Binary.u16 table (at + 2)) }, index + 1)

    /// <summary>Reads one context rule, <c>Rule</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The input value width.</param>
    /// <returns>The rule.</returns>
    let private parseRule (table: ReadOnlySpan<byte>) rule vw =
        let inputCount = int (Binary.u16 table rule)
        let lookupCount = int (Binary.u16 table (rule + 2))
        let inputLength = max 0 (inputCount - 1)
        let input = Sealed.unfoldSpan table inputLength 0 { GlyphArrayDecoder.Base = rule + 4; Width = vw }
        let records = Sealed.unfoldSpan table lookupCount 0 { LookupRecordDecoder.Base = rule + 4 + inputLength * vw }
        { InputCount = inputCount; Input = input; Records = records }

    /// <summary>Unfolds the rules of one context rule set.</summary>
    [<Struct>]
    type private RuleSetDecoder =
        { SetOffset: int
          RuleWidth: int
          ValueWidth: int }
        interface ISpanUnfolder<byte, int, Rule> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.SetOffset + 2 + index * this.RuleWidth) this.RuleWidth
                match offset with
                | 0u -> struct ({ InputCount = 0; Input = ImmutableArray.Empty; Records = ImmutableArray.Empty }, index + 1)
                | _ -> struct (parseRule table (this.SetOffset + int offset) this.ValueWidth, index + 1)

    /// <summary>Unfolds the rule sets of a context subtable.</summary>
    [<Struct>]
    type private RuleSetArrayDecoder =
        { Subtable: int
          SetsBase: int
          SetWidth: int
          RuleWidth: int
          ValueWidth: int }
        interface ISpanUnfolder<byte, int, ImmutableArray<Rule>> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.SetsBase + index * this.SetWidth) this.SetWidth
                match offset with
                | 0u -> struct (ImmutableArray.Empty, index + 1)
                | _ ->
                    let at = this.Subtable + int offset
                    struct (Sealed.unfoldSpan table (int (Binary.u16 table at)) 0 { RuleSetDecoder.SetOffset = at; RuleWidth = this.RuleWidth; ValueWidth = this.ValueWidth }, index + 1)

    /// <summary>Unfolds a coverage-offset array.</summary>
    [<Struct>]
    type private CoverageArrayDecoder =
        { Subtable: int
          Base: int
          Width: int }
        interface ISpanUnfolder<byte, int, Coverage> with
            member this.Next (table, index) =
                struct (coverageReached table this.Subtable (Coverage.readWide table (this.Base + index * this.Width) this.Width), index + 1)

    /// <summary>The digest of the first input values of one rule set, <c>collect_first_input_classes</c>: a bit per
    /// value modulo 64, all bits when a rule has no glyph after the first.</summary>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private firstClassDigest (rules: ImmutableArray<Rule>) index digest =
        match index < rules.Length with
        | false -> digest
        | true ->
            let rule = rules[index]
            match rule.InputCount <= 1 with
            | true -> UInt64.MaxValue
            | false -> firstClassDigest rules (index + 1) (digest ||| (1UL <<< int (rule.Input[0] &&& 63u)))

    /// <summary>The digest of the first input values of one chained rule set, <c>collect_first_input_classes</c>.</summary>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest.</returns>
    [<TailCall>]
    let rec private firstChainClassDigest (rules: ImmutableArray<ChainRule>) index digest =
        match index < rules.Length with
        | false -> digest
        | true ->
            let rule = rules[index]
            match rule.ChainInputCount <= 1 with
            | true -> UInt64.MaxValue
            | false -> firstChainClassDigest rules (index + 1) (digest ||| (1UL <<< int (rule.ChainInput[0] &&& 63u)))

    /// <summary>The per-rule-set first-class digests of a class rule subtable.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <returns>The digests, one per set.</returns>
    let private firstClassDigests (ruleSets: ImmutableArray<ImmutableArray<Rule>>) =
        Sealed.init ruleSets.Length (fun index -> firstClassDigest ruleSets[index] 0 0UL)

    /// <summary>The per-rule-set first-class digests of a chained class rule subtable.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <returns>The digests, one per set.</returns>
    let private firstChainClassDigests (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) =
        Sealed.init ruleSets.Length (fun index -> firstChainClassDigest ruleSets[index] 0 0UL)

    /// <summary>Reads a context subtable, formats 1-6, <c>Context</c>, the one reader both GSUB (type 5) and GPOS (type 7) use.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The rules, none for a format that is not one of the six.</returns>
    let contextRules (table: ReadOnlySpan<byte>) o =
        match int (Binary.u16 table o) with
        | 1 | 4 ->
            let covW = if int (Binary.u16 table o) = 4 then 4 else 2
            let w = if int (Binary.u16 table o) = 4 then Width24 else Width16
            let coverage = coverageReached table o (coverageOffset table (o + 2) covW)
            let count = int (Coverage.readWide table (o + 2 + covW) w)
            let ruleSets = Sealed.unfoldSpan table count 0 { RuleSetArrayDecoder.Subtable = o; SetsBase = o + 2 + covW + w; SetWidth = w; RuleWidth = Width16; ValueWidth = w }
            ValueSome (GlyphRules (coverage, ruleSets))
        | 2 | 5 ->
            let covW = if int (Binary.u16 table o) = 5 then 4 else 2
            let w = if int (Binary.u16 table o) = 5 then Width24 else Width16
            let coverage = coverageReached table o (coverageOffset table (o + 2) covW)
            let classDef = classDefReached table o (coverageOffset table (o + 2 + covW) covW)
            let count = int (Coverage.readWide table (o + 2 + 2 * covW) w)
            let ruleSets = Sealed.unfoldSpan table count 0 { RuleSetArrayDecoder.Subtable = o; SetsBase = o + 2 + 2 * covW + w; SetWidth = w; RuleWidth = w; ValueWidth = Width16 }
            ValueSome (ClassRules (coverage, classDef, ruleSets, firstClassDigests ruleSets))
        | 3 | 6 ->
            let w = if int (Binary.u16 table o) = 6 then Width24 else Width16
            let glyphCount = int (Binary.u16 table (o + 2))
            let lookupCount = int (Binary.u16 table (o + 4))
            let coverages = Sealed.unfoldSpan table glyphCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 6; Width = w }
            let records = Sealed.unfoldSpan table lookupCount 0 { LookupRecordDecoder.Base = o + 6 + glyphCount * w }
            ValueSome (CoverageRule (coverages, records))
        | _ -> ValueNone

    /// <summary>Parses a GSUB context subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseContext (table: ReadOnlySpan<byte>) o =
        match contextRules table o with
        | ValueSome rules -> SubstContext rules
        | ValueNone -> SubstPending (ContextType, int (Binary.u16 table o), Digest.empty, Digest.full)

    /// <summary>Reads one chained rule, <c>ChainRule</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The value width.</param>
    /// <returns>The rule.</returns>
    let private parseChainRule (table: ReadOnlySpan<byte>) rule vw =
        let backtrackCount = int (Binary.u16 table rule)
        let backtrack = Sealed.unfoldSpan table backtrackCount 0 { GlyphArrayDecoder.Base = rule + 2; Width = vw }
        let inputCount = int (Binary.u16 table (rule + 2 + backtrackCount * vw))
        let inputLength = max 0 (inputCount - 1)
        let input = Sealed.unfoldSpan table inputLength 0 { GlyphArrayDecoder.Base = rule + 4 + backtrackCount * vw; Width = vw }
        let lookaheadAt = rule + 4 + backtrackCount * vw + inputLength * vw
        let lookaheadCount = int (Binary.u16 table lookaheadAt)
        let lookahead = Sealed.unfoldSpan table lookaheadCount 0 { GlyphArrayDecoder.Base = lookaheadAt + 2; Width = vw }
        let lookupAt = lookaheadAt + 2 + lookaheadCount * vw
        let lookupCount = int (Binary.u16 table lookupAt)
        let records = Sealed.unfoldSpan table lookupCount 0 { LookupRecordDecoder.Base = lookupAt + 2 }
        { Backtrack = backtrack; ChainInputCount = inputCount; ChainInput = input; Lookahead = lookahead; ChainRecords = records }

    /// <summary>Unfolds the rules of one chained rule set.</summary>
    [<Struct>]
    type private ChainRuleSetDecoder =
        { SetOffset: int
          RuleWidth: int
          ValueWidth: int }
        interface ISpanUnfolder<byte, int, ChainRule> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.SetOffset + 2 + index * this.RuleWidth) this.RuleWidth
                match offset with
                | 0u -> struct ({ Backtrack = ImmutableArray.Empty; ChainInputCount = 0; ChainInput = ImmutableArray.Empty; Lookahead = ImmutableArray.Empty; ChainRecords = ImmutableArray.Empty }, index + 1)
                | _ -> struct (parseChainRule table (this.SetOffset + int offset) this.ValueWidth, index + 1)

    /// <summary>Unfolds the rule sets of a chained subtable.</summary>
    [<Struct>]
    type private ChainRuleSetArrayDecoder =
        { Subtable: int
          SetsBase: int
          SetWidth: int
          RuleWidth: int
          ValueWidth: int }
        interface ISpanUnfolder<byte, int, ImmutableArray<ChainRule>> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.SetsBase + index * this.SetWidth) this.SetWidth
                match offset with
                | 0u -> struct (ImmutableArray.Empty, index + 1)
                | _ ->
                    let at = this.Subtable + int offset
                    struct (Sealed.unfoldSpan table (int (Binary.u16 table at)) 0 { ChainRuleSetDecoder.SetOffset = at; RuleWidth = this.RuleWidth; ValueWidth = this.ValueWidth }, index + 1)

    /// <summary>Reads a chained-context subtable, formats 1-5, <c>ChainContext</c>, the one reader both GSUB (type 6) and GPOS
    /// (type 8) use.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The rules, none for a format that is not one of the five.</returns>
    let chainRules (table: ReadOnlySpan<byte>) o =
        match int (Binary.u16 table o) with
        | 1 | 4 ->
            let covW = if int (Binary.u16 table o) = 4 then 4 else 2
            let w = if int (Binary.u16 table o) = 4 then Width24 else Width16
            let coverage = coverageReached table o (coverageOffset table (o + 2) covW)
            let count = int (Coverage.readWide table (o + 2 + covW) w)
            let ruleSets = Sealed.unfoldSpan table count 0 { ChainRuleSetArrayDecoder.Subtable = o; SetsBase = o + 2 + covW + w; SetWidth = w; RuleWidth = w; ValueWidth = w }
            ValueSome (ChainGlyphRules (coverage, ruleSets))
        | 2 | 5 ->
            let covW = if int (Binary.u16 table o) = 5 then 4 else 2
            let w = if int (Binary.u16 table o) = 5 then Width24 else Width16
            let coverage = coverageReached table o (coverageOffset table (o + 2) covW)
            let backtrackClassDef = classDefReached table o (coverageOffset table (o + 2 + covW) covW)
            let inputClassDef = classDefReached table o (coverageOffset table (o + 2 + 2 * covW) covW)
            let lookaheadClassDef = classDefReached table o (coverageOffset table (o + 2 + 3 * covW) covW)
            let count = int (Coverage.readWide table (o + 2 + 4 * covW) w)
            let ruleSets = Sealed.unfoldSpan table count 0 { ChainRuleSetArrayDecoder.Subtable = o; SetsBase = o + 2 + 4 * covW + w; SetWidth = w; RuleWidth = w; ValueWidth = Width16 }
            ValueSome (ChainClassRules (coverage, backtrackClassDef, inputClassDef, lookaheadClassDef, ruleSets, firstChainClassDigests ruleSets))
        | 3 ->
            let w = Width16
            let backtrackCount = int (Binary.u16 table (o + 2))
            let backtrack = Sealed.unfoldSpan table backtrackCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 4; Width = w }
            let inputCount = int (Binary.u16 table (o + 4 + backtrackCount * w))
            let input = Sealed.unfoldSpan table inputCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 6 + backtrackCount * w; Width = w }
            let lookaheadCount = int (Binary.u16 table (o + 6 + backtrackCount * w + inputCount * w))
            let lookahead = Sealed.unfoldSpan table lookaheadCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 8 + backtrackCount * w + inputCount * w; Width = w }
            let lookupCount = int (Binary.u16 table (o + 8 + backtrackCount * w + inputCount * w + lookaheadCount * w))
            let records = Sealed.unfoldSpan table lookupCount 0 { LookupRecordDecoder.Base = o + 10 + backtrackCount * w + inputCount * w + lookaheadCount * w }
            ValueSome (ChainCoverageRule (backtrack, input, lookahead, records))
        | _ -> ValueNone

    /// <summary>Parses a GSUB chained-context subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseChain (table: ReadOnlySpan<byte>) o =
        match chainRules table o with
        | ValueSome rules -> SubstChainContext rules
        | ValueNone -> SubstPending (ChainType, int (Binary.u16 table o), Digest.empty, Digest.full)

    /// <summary>Parses a reverse-chaining single subtable, formats 1-2, <c>ReverseChainSingleSubst</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseReverse (table: ReadOnlySpan<byte>) o =
        match int (Binary.u16 table o) with
        | 1 | 2 ->
            let covW = if int (Binary.u16 table o) = 2 then 4 else 2
            let w = if int (Binary.u16 table o) = 2 then Width24 else Width16
            let coverage = coverageReached table o (coverageOffset table (o + 2) covW)
            let backtrackCount = int (Binary.u16 table (o + 2 + covW))
            let backtrack = Sealed.unfoldSpan table backtrackCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 4 + covW; Width = w }
            let lookaheadCount = int (Binary.u16 table (o + 4 + covW + backtrackCount * w))
            let lookahead = Sealed.unfoldSpan table lookaheadCount 0 { CoverageArrayDecoder.Subtable = o; Base = o + 6 + covW + backtrackCount * w; Width = w }
            let glyphCount = int (Coverage.readWide table (o + 6 + covW + backtrackCount * w + lookaheadCount * w) w)
            let substitutes = Sealed.unfoldSpan table glyphCount 0 { GlyphArrayDecoder.Base = o + 6 + covW + backtrackCount * w + lookaheadCount * w + w; Width = w }
            ReverseChainSingle (coverage, backtrack, lookahead, substitutes)
        | format -> SubstPending (ReverseType, format, Digest.empty, Digest.full)

    /// <summary>Parses one GSUB subtable, unwrapping extensions to the extended type.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="lookupType">The lookup's type.</param>
    /// <returns>The subtable.</returns>
    let rec parse (table: ReadOnlySpan<byte>) offset lookupType =
        match lookupType with
        | ExtensionType ->
            let innerType = int (Binary.u16 table (offset + 2))
            match Binary.u32 table (offset + 4) with
            | 0u -> SubstPending (ExtensionType, 1, Digest.empty, Digest.full)
            | innerOffset -> parse table (offset + int innerOffset) innerType
        | SingleType -> parseSingle table offset
        | MultipleType -> parseMultiple table offset
        | AlternateType -> parseAlternate table offset
        | LigatureType -> parseLigatureSubst table offset
        | ContextType -> parseContext table offset
        | ChainType -> parseChain table offset
        | ReverseType -> parseReverse table offset
        | _ ->
            let format = int (Binary.u16 table offset)
            let struct (coverage, second) = SubtableDigest.collect table offset lookupType true
            SubstPending (lookupType, format, coverage, second)
