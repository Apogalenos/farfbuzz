namespace Farfbuzz

open System.Collections.Immutable

/// <summary>One glyph rule: the input sequence after the coverage glyph and its lookup records, <c>Rule</c>.</summary>
[<Struct>]
type internal Rule =
    { InputCount: int // as stored: 0 and 1 both mean no glyph after the first, but only 1 lets a record for the first glyph run
      Input: ImmutableArray<uint32>
      Records: ImmutableArray<LookupRecord> }

/// <summary>One chained glyph rule: the backtrack, input and lookahead sequences and its records, <c>ChainRule</c>.</summary>
[<Struct>]
type internal ChainRule =
    { Backtrack: ImmutableArray<uint32>
      ChainInputCount: int // as stored, see Rule
      ChainInput: ImmutableArray<uint32>
      Lookahead: ImmutableArray<uint32>
      ChainRecords: ImmutableArray<LookupRecord> }

/// <summary>The context rules of a GSUB or GPOS subtable, formats 1-6, <c>Context</c>.</summary>
type internal ContextRules =
    | GlyphRules of glyphCoverage: Coverage * glyphRuleSets: ImmutableArray<ImmutableArray<Rule>>
    | ClassRules of classCoverage: Coverage * classDef: ClassDef * classRuleSets: ImmutableArray<ImmutableArray<Rule>> * firstClassDigests: ImmutableArray<uint64>
    | CoverageRule of inputCoverages: ImmutableArray<Coverage> * coverageRecords: ImmutableArray<LookupRecord>

/// <summary>The chained context rules of a GSUB or GPOS subtable, formats 1-5, <c>ChainContext</c>.</summary>
type internal ChainRules =
    | ChainGlyphRules of chainGlyphCoverage: Coverage * chainGlyphRuleSets: ImmutableArray<ImmutableArray<ChainRule>>
    | ChainClassRules of chainClassCoverage: Coverage * backtrackClasses: ClassDef * inputClasses: ClassDef * lookaheadClasses: ClassDef * chainClassRuleSets: ImmutableArray<ImmutableArray<ChainRule>> * chainFirstClassDigests: ImmutableArray<uint64>
    | ChainCoverageRule of backtrackCoverages: ImmutableArray<Coverage> * chainInputCoverages: ImmutableArray<Coverage> * lookaheadCoverages: ImmutableArray<Coverage> * chainCoverageRecords: ImmutableArray<LookupRecord>
