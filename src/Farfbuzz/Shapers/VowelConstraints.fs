namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The vowel-constraint preprocess pass of <c>hb-ot-shaper-vowel-constraints.cc</c>.</summary>
[<RequireQualifiedAccess>]
module internal VowelConstraints =
    /// <summary>The dotted circle inserted between prohibited vowel sequences, <c>0x25CC</c>.</summary>
    [<Literal>]
    let private DottedCircle = 0x25CCu

    /// <summary>The outcome of scanning one glyph's constraints.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Match =
        | NoMatch
        | PairMatch   // note: a two-code-point prohibited sequence
        | TripleMatch // note: a three-code-point prohibited sequence

    /// <summary>The state of the vowel-constraint pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private State =
        | VcOpen
        | VcLoop
        | VcAdvanceThird
        | VcDottedCircle
        | VcDottedNext
        | VcDone

    /// <summary>Classifies the third code point of a matched two-code-point prefix.</summary>
    /// <param name="third">The third code point, zero for a two-code-point sequence.</param>
    /// <param name="index">The current input index.</param>
    /// <param name="infos">The input glyphs.</param>
    /// <returns>The match, or <c>NoMatch</c> when the third code point does not follow.</returns>
    let private classifyThird (third: uint32) (index: int) (infos: ReadOnlySpan<GlyphInfo>) : Match =
        match third = 0u with
        | true -> PairMatch
        | false ->
            match index + 2 < infos.Length && infos[index + 2].Codepoint = third with
            | true -> TripleMatch
            | false -> NoMatch

    /// <summary>Finds the prohibited sequence matching <paramref name="cur"/> then <paramref name="nxt"/>.</summary>
    /// <param name="constraints">The script's minimal prohibited sequences.</param>
    /// <param name="infos">The input glyphs.</param>
    /// <param name="index">The current input index.</param>
    /// <param name="cur">The current code point.</param>
    /// <param name="nxt">The next code point.</param>
    /// <param name="candidate">The next constraint to test.</param>
    /// <returns>The match.</returns>
    [<TailCall>]
    let rec private findMatch
            (constraints: ImmutableArray<struct (uint32 * uint32 * uint32)>)
            (infos: ReadOnlySpan<GlyphInfo>)
            (index: int)
            (cur: uint32)
            (nxt: uint32)
            (candidate: int)
            : Match =
        match candidate >= constraints.Length with
        | true -> NoMatch
        | false ->
            let struct (a, b, c) = constraints[candidate]
            match a = cur && b = nxt with
            | false -> findMatch constraints infos index cur nxt (candidate + 1)
            | true ->
                match classifyThird c index infos with
                | NoMatch -> findMatch constraints infos index cur nxt (candidate + 1)
                | PairMatch -> PairMatch
                | TripleMatch -> TripleMatch

    /// <summary>The vowel-constraint pass as an out-buffer pass.</summary>
    [<Struct>]
    type private Pass =
        val private Constraints: ImmutableArray<struct (uint32 * uint32 * uint32)>

        /// <summary>Creates a vowel-constraint pass.</summary>
        /// <param name="constraints">The script's minimal prohibited sequences.</param>
        new (constraints) = { Constraints = constraints }

        interface IRunPass<State> with
            member this.Step (cursor, state) =
                match state with
                | VcOpen -> struct (RunEdit.ClearOutput, VcLoop)
                | VcLoop ->
                    match cursor.Index + 1 < cursor.Input.Length && cursor.Successful with
                    | false -> struct (RunEdit.Finish, VcDone)
                    | true ->
                        let cur = cursor.Input[cursor.Index].Codepoint
                        let nxt = cursor.Input[cursor.Index + 1].Codepoint
                        match findMatch this.Constraints cursor.Input cursor.Index cur nxt 0 with
                        | NoMatch -> struct (RunEdit.NextGlyph, VcLoop)
                        | PairMatch -> struct (RunEdit.NextGlyph, VcDottedCircle)
                        | TripleMatch -> struct (RunEdit.NextGlyph, VcAdvanceThird)
                | VcAdvanceThird -> struct (RunEdit.NextGlyph, VcDottedCircle)
                | VcDottedCircle ->
                    let cur = cursor.Input[cursor.Index]
                    let dotted = { cur with Codepoint = DottedCircle }
                    let props = UnicodeProps.clearContinuation cursor.InputProps[cursor.Index]
                    struct (RunEdit.OutputInfo (dotted, props), VcDottedNext)
                | VcDottedNext -> struct (RunEdit.NextGlyph, VcLoop)
                | VcDone -> struct (RunEdit.Finish, VcDone)

    /// <summary>Inserts dotted circles into prohibited vowel sequences, <c>_hb_preprocess_text_vowel_constraints</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with dotted circles inserted before the last code point of every matched sequence.</returns>
    let preprocess (run: GlyphRun) : GlyphRun =
        match run.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default with
        | false -> run
        | true ->
            let constraints = VowelConstraintTable.forScript run.SegmentProperties.Script
            GlyphRun.runPass (Pass(constraints)) VcOpen run

    /// <summary>The scripts whose table is not empty, read once: <see cref="VowelConstraintTable.forScript"/> builds its table on every
    /// call, and a script that is not in this list takes it from there, whose answer is the same.</summary>
    let private tabledScripts =
        [| Scripts.Devanagari; Scripts.Bengali; Scripts.Gurmukhi; Scripts.Gujarati; Scripts.Oriya; Scripts.Tamil; Scripts.Telugu
           Scripts.Kannada; Scripts.Malayalam; Scripts.Sinhala; Scripts.Brahmi; Scripts.Khojki; Scripts.Khudawadi; Scripts.Tirhuta
           Scripts.Modi; Scripts.Takri |]

    /// <summary>The bits of the first and of the second code points of a table, one bit for the low six bits of each: a pair whose first or
    /// second code point's bit is clear starts no prohibited sequence.</summary>
    /// <param name="constraints">The script's minimal prohibited sequences.</param>
    /// <param name="index">The next sequence.</param>
    /// <param name="first">The bits of the first code points so far.</param>
    /// <param name="second">The bits of the second code points so far.</param>
    /// <returns>The two filters.</returns>
    [<TailCall>]
    let rec private filtersFrom (constraints: ImmutableArray<struct (uint32 * uint32 * uint32)>) (index: int) (first: uint64) (second: uint64) =
        match index < constraints.Length with
        | false -> struct (first, second)
        | true ->
            let struct (a, b, _) = constraints[index]
            filtersFrom constraints (index + 1) (first ||| (1UL <<< int (a &&& 63u))) (second ||| (1UL <<< int (b &&& 63u)))

    /// <summary>The first-code-point and second-code-point filters of a table.</summary>
    /// <param name="constraints">The script's minimal prohibited sequences.</param>
    /// <returns>The table and its two filters.</returns>
    let private withFilters (constraints: ImmutableArray<struct (uint32 * uint32 * uint32)>) =
        let struct (first, second) = filtersFrom constraints 0 0UL 0UL
        struct (constraints, first, second)

    /// <summary>The tables of <see cref="tabledScripts"/> with their filters, built when first read: a pure value that cannot
    /// fail.</summary>
    let private tables =
        lazy
            (tabledScripts
             |> Array.map (fun script ->
                 withFilters (VowelConstraintTable.forScript script)))

    /// <summary>Finds a script in the list of the scripts that have a table.</summary>
    /// <param name="script">The script.</param>
    /// <param name="index">The next script of the list.</param>
    /// <returns>The index, or the length of the list when the script has no table.</returns>
    [<TailCall>]
    let rec private tabledIndex (script: Script) (index: int) =
        match index < tabledScripts.Length && tabledScripts[index] <> script with
        | true -> tabledIndex script (index + 1)
        | false -> index

    /// <summary>The prohibited sequences of a script and their filter: <see cref="VowelConstraintTable.forScript"/> without building the table again.</summary>
    /// <param name="script">The script.</param>
    /// <returns>The table and its filters of the first and the second code points.</returns>
    let private constraintsOf (script: Script) =
        match tabledIndex script 0 with
        | index when index < tabledScripts.Length -> tables.Value[index]
        | _ -> withFilters (VowelConstraintTable.forScript script)

    /// <summary>Whether any pair of neighbouring code points of the run starts a prohibited sequence.</summary>
    /// <param name="constraints">The script's minimal prohibited sequences.</param>
    /// <param name="filter">The first-code-point filter of the table.</param>
    /// <param name="secondary">The second-code-point filter of the table.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when <see cref="Pass"/> would insert a dotted circle.</returns>
    [<TailCall>]
    let rec private anyMatch (constraints: ImmutableArray<struct (uint32 * uint32 * uint32)>) (filter: uint64) (secondary: uint64) (infos: ReadOnlySpan<GlyphInfo>) (index: int) =
        match index + 1 < infos.Length with
        | false -> false
        | true ->
            let cur = infos[index].Codepoint
            let nxt = infos[index + 1].Codepoint
            match (filter >>> int (cur &&& 63u)) &&& (secondary >>> int (nxt &&& 63u)) &&& 1UL with
            | 0UL -> anyMatch constraints filter secondary infos (index + 1)
            | _ ->
                match findMatch constraints infos index cur nxt 0 with
                | NoMatch -> anyMatch constraints filter secondary infos (index + 1)
                | PairMatch | TripleMatch -> true

    /// <summary>Inserts dotted circles into prohibited vowel sequences as <see cref="preprocess"/> does, but a run that holds no
    /// prohibited sequence is returned as it is, found by one scan that allocates nothing: the pass would copy it unchanged.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with dotted circles inserted before the last code point of every matched sequence.</returns>
    let preprocessScanned (run: GlyphRun) =
        match run.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default with
        | false -> run
        | true ->
            let struct (constraints, filter, secondary) = constraintsOf run.SegmentProperties.Script
            match anyMatch constraints filter secondary (run.Infos.AsSpan()) 0 with
            | false -> run
            | true -> GlyphRun.runPass (Pass(constraints)) VcOpen run

    /// <summary>Inserts the dotted circles of <see cref="preprocessScanned"/> into a working set: a set with no prohibited sequence is
    /// returned as it is, and the pass runs on a run for the others.</summary>
    /// <param name="set">The working set.</param>
    /// <returns>The set with dotted circles inserted before the last code point of every matched sequence.</returns>
    let preprocessScannedSet (set: WorkingSet) =
        match set.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default with
        | false -> set
        | true ->
            let struct (constraints, filter, secondary) = constraintsOf set.SegmentProperties.Script
            match anyMatch constraints filter secondary (set.Infos.AsSpan()) 0 with
            | false -> set
            | true -> GlyphRun.setOf (GlyphRun.runPass (Pass(constraints)) VcOpen (GlyphRun.runOf set))
