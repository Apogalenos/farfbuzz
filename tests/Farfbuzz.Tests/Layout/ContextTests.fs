module Farfbuzz.Tests.Layout.ContextTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz

/// <summary>A glyph info with a code point and mask.</summary>
/// <param name="codepoint">The glyph id.</param>
/// <param name="mask">The mask.</param>
/// <returns>The glyph info.</returns>
let private infoOf (codepoint: uint32) (mask: uint32) =
    { Codepoint = codepoint; Mask = mask; Cluster = 0u; Var1 = 0u; Var2 = 0u }

/// <summary>Builds a run of the given glyph ids, every glyph under mask 1 and one cluster per glyph.</summary>
/// <param name="glyphIds">The glyph ids.</param>
/// <returns>The run.</returns>
let private runOf (glyphIds: uint32 array) =
    let infos = Sealed.init glyphIds.Length (fun index -> { Codepoint = glyphIds[index]; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init glyphIds.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props; ContentType = GlyphContent }

/// <summary>The glyph ids of a shaped run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyph ids.</returns>
let private glyphIdsOf (run: GlyphRun) =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)

/// <summary>Builds a coverage over one glyph.</summary>
/// <param name="glyph">The glyph.</param>
/// <returns>The coverage.</returns>
let private coverageOf (glyph: uint32) =
    CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| glyph |]))

/// <summary>Builds a coverage over a list of glyphs.</summary>
/// <param name="glyphs">The glyphs, sorted and distinct.</param>
/// <returns>The coverage.</returns>
let private coveragesOf (glyphs: uint32 list) =
    CoverageGlyphs (Sealed.ofList glyphs)

/// <summary>Builds a lookup accelerator from one subtable.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="isReverse">Whether the lookup is reverse.</param>
/// <param name="subtable">The subtable.</param>
/// <returns>The accelerator.</returns>
let private accelOf (lookupType: int) (isReverse: bool) (subtable: SubstSubtable) =
    LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
        { LookupType = lookupType
          Props = 0u
          IsReverse = isReverse
          Subtables = ImmutableArray.Create subtable }

/// <summary>Builds a lookup accelerator from several subtables.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="isReverse">Whether the lookup is reverse.</param>
/// <param name="subtables">The subtables.</param>
/// <returns>The accelerator.</returns>
let private accelOfMany (lookupType: int) (isReverse: bool) (subtables: SubstSubtable list) =
    LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
        { LookupType = lookupType
          Props = 0u
          IsReverse = isReverse
          Subtables = ImmutableArray.CreateRange subtables }

/// <summary>Builds a GSUB table over the given lookup accelerators.</summary>
/// <param name="lookups">The lookups, in table order.</param>
/// <returns>The table.</returns>
let private tableOf (lookups: LookupAccel<SubstSubtable> list) =
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = lookups |> List.map (fun lookup -> lazy lookup) |> ImmutableArray.CreateRange
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>Applies the lookups at the given table indices through the kernel, each under mask 1.</summary>
/// <param name="table">The table.</param>
/// <param name="mapIndices">The table indices of the lookups to apply, in order.</param>
/// <param name="run">The run.</param>
/// <returns>The substituted run.</returns>
let private apply (table: LayoutTable<SubstSubtable>) (mapIndices: int array) (run: GlyphRun) =
    let maps =
        Sealed.init mapIndices.Length (fun index ->
            { LookupIndex = mapIndices[index]
              AutoZwnj = false
              AutoZwj = false
              Random = false
              PerSyllable = false
              Mask = 1u
              FeatureTag = Tag.make 'c' 'c' 'm' 'p' })
    let stages = ImmutableArray.Create { LastLookup = mapIndices.Length; Pause = PauseKind.NoPause }
    let font = Font.create Face.empty
    Engine.applyGsub true table maps stages font run

/// <summary>The P5.S09 context, chained-context, nested-lookup and reverse-chaining cases.</summary>
[<Tests>]
let tests =
    testList "P5.S09" [
        test "apply_lookup growth: a nested multiple substitution grows the run and shifts the positions" {
            let multiple = Multiple (coverageOf 1u, Sealed.ofSpan (ReadOnlySpan [| Sealed.ofList [ 10u; 11u; 12u ] |]))
            let context =
                SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 1u), ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }))
            let table = tableOf [ accelOf 2 false multiple; accelOf 5 false context ]
            let result = apply table [| 1 |] (runOf [| 1u; 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 10u; 11u; 12u; 2u; 3u |] "glyph 1 becomes three glyphs"
        }

        test "apply_lookup shrink: a nested deletion clamps the end so it never rewinds past the position" {
            let delete = Multiple (coverageOf 1u, Sealed.ofSpan (ReadOnlySpan [| (ImmutableArray.Empty: ImmutableArray<uint32>) |]))
            let context =
                SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 1u), ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }))
            let table = tableOf [ accelOf 2 false delete; accelOf 5 false context ]
            let result = apply table [| 1 |] (runOf [| 1u; 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 2u; 3u |] "glyph 1 is deleted"
        }

        test "nesting limit: a lookup that recurses into itself stops at the limit" {
            let recursive =
                SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 1u), ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }))
            let table = tableOf [ accelOf 5 false recursive ]
            let result = apply table [| 0 |] (runOf [| 1u; 2u |])
            Expect.isFalse result.Successful "the nesting limit makes the buffer fail"
        }

        test "a five-rule glyph rule set matches the right rule" {
            let rule input recordLookup =
                { InputCount = List.length input + 1; Input = Sealed.ofList input; Records = ImmutableArray.Create { SequenceIndex = 1; LookupListIndex = recordLookup } }
            let rules =
                ImmutableArray.CreateRange
                    [ rule [ 9u ] 1
                      rule [ 2u ] 0
                      rule [ 3u ] 1
                      rule [ 4u ] 1
                      rule [ 5u ] 1 ]
            let ruleSets = ImmutableArray.Create rules
            let single0 = SingleList (coverageOf 2u, Sealed.ofSpan (ReadOnlySpan [| 20u |]))
            let single1 = SingleList (coverageOf 2u, Sealed.ofSpan (ReadOnlySpan [| 99u |]))
            let context = SubstContext (GlyphRules (coverageOf 1u, ruleSets))
            let table = tableOf [ accelOf 1 false single0; accelOf 1 false single1; accelOf 5 false context ]
            let result = apply table [| 2 |] (runOf [| 1u; 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 1u; 20u; 3u |] "the second rule's nested single substitution applies"
        }

        test "class matching: a class rule set matches by class" {
            let classDef = ClassDefArray (1u, Sealed.ofList [ 1u; 1u; 0u ])
            let rule =
                { InputCount = 2; Input = Sealed.ofList [ 1u ]; Records = ImmutableArray.Create { SequenceIndex = 1; LookupListIndex = 0 } }
            let ruleSets = ImmutableArray.Create (ImmutableArray.Empty, ImmutableArray.Create rule)
            let firstClassDigests = Sealed.init 2 (fun _ -> 0UL)
            let single = SingleList (coverageOf 2u, Sealed.ofSpan (ReadOnlySpan [| 30u |]))
            let context = SubstContext (ClassRules (coverageOf 1u, classDef, ruleSets, firstClassDigests))
            let table = tableOf [ accelOf 1 false single; accelOf 5 false context ]
            let result = apply table [| 1 |] (runOf [| 1u; 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 1u; 30u; 3u |] "the class rule's nested single substitution applies"
        }

        test "coverage-format rule: format 3 matches a coverage sequence" {
            let single = SingleList (coverageOf 2u, Sealed.ofSpan (ReadOnlySpan [| 40u |]))
            let context =
                SubstContext (CoverageRule (Sealed.ofList [ coverageOf 1u; coverageOf 2u ], ImmutableArray.Create { SequenceIndex = 1; LookupListIndex = 0 }))
            let table = tableOf [ accelOf 1 false single; accelOf 5 false context ]
            let result = apply table [| 1 |] (runOf [| 1u; 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 1u; 40u; 3u |] "the coverage sequence matches and substitutes the second glyph"
        }

        test "a chain of nested lookups applies up to HB_MAX_NESTING_LEVEL and fails one level deeper" {
            let coverage = coverageOf 1u
            let call next = SubstContext (CoverageRule (ImmutableArray.Create coverage, ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = next }))
            let single = SingleDelta (coverage, 1u, 0xFFFFu)
            let chain depth =
                let contexts = [ for index in 0 .. depth - 1 -> accelOf 5 false (call (index + 1)) ]
                tableOf (contexts @ [ accelOf 1 false single ])
            for depth in [ 1; 2; 62; 63; 64 ] do
                let result = apply (chain depth) [| 0 |] (runOf [| 1u; 2u |])
                Expect.equal (glyphIdsOf result) [| 2u; 2u |] $"{depth} nested lookups still reach the substitution"
                Expect.isTrue result.Successful $"{depth} nested lookups do not fail the buffer"
            for depth in [ 65; 66; 70 ] do
                let result = apply (chain depth) [| 0 |] (runOf [| 1u; 2u |])
                Expect.equal (glyphIdsOf result) [| 1u; 2u |] $"{depth} nested lookups never reach the substitution"
                Expect.isFalse result.Successful $"{depth} nested lookups reach the nesting limit and fail the buffer"
        }

        test "the operation budget stops the records of a rule: a generous budget applies every record, a spent one applies fewer" {
            let single = SingleDelta (CoverageRanges (ImmutableArray.Create { First = 1u; Last = 40u; Value = 0u }), 1u, 0xFFFFu)
            let rule count = SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 1u), ImmutableArray.CreateRange (List.replicate count { SequenceIndex = 0; LookupListIndex = 0 })))
            let appliedBy count budget =
                let table = tableOf [ accelOf 1 false single; accelOf 5 false (rule count) ]
                let result = apply table [| 1 |] { runOf [| 1u; 9u |] with MaxOps = budget }
                (glyphIdsOf result)[0] - 1u
            for count in [ 1; 3; 10 ] do
                Expect.equal (appliedBy count GlyphRun.MaxOpsDefault) (uint32 count) $"{count} records with the default budget all apply"
            Expect.equal (appliedBy 10 0) 0u "a budget of zero applies no record (apply_lookup breaks on max_ops <= 0, src/hb-ot-layout-gsubgpos.hh:2767)"
            Expect.isLessThan (appliedBy 10 1) 10u "a budget of one is spent before the tenth record"
            Expect.isLessThan (appliedBy 10 3) 10u "a budget of three is spent before the tenth record"
        }

        test "edges: an empty run, one glyph, a match at the end of the run, a lookahead that runs out, a backtrack that starts the run" {
            let single = SingleDelta (coverageOf 2u, 5u, 0xFFFFu)
            let chain backtrack lookahead =
                SubstChainContext (ChainCoverageRule (Sealed.ofList backtrack, ImmutableArray.Create (coverageOf 2u), Sealed.ofList lookahead, ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }))
            let table backtrack lookahead = tableOf [ accelOf 1 false single; accelOf 6 false (chain backtrack lookahead) ]
            Expect.equal (glyphIdsOf (apply (table [] []) [| 1 |] (runOf [||]))) [||] "an empty run"
            Expect.equal (glyphIdsOf (apply (table [] []) [| 1 |] (runOf [| 2u |]))) [| 7u |] "one glyph"
            Expect.equal (glyphIdsOf (apply (table [ coverageOf 1u ] []) [| 1 |] (runOf [| 1u; 2u |]))) [| 1u; 7u |] "a match that ends at the end of the run"
            Expect.equal (glyphIdsOf (apply (table [] [ coverageOf 3u ]) [| 1 |] (runOf [| 2u |]))) [| 2u |] "a lookahead longer than the rest of the run"
            Expect.equal (glyphIdsOf (apply (table [ coverageOf 1u ] []) [| 1 |] (runOf [| 2u; 3u |]))) [| 2u; 3u |] "a backtrack that starts the run"
            let result = apply (table [] [ coverageOf 3u ]) [| 1 |] (runOf [| 2u; 3u |])
            Expect.equal (glyphIdsOf result) [| 7u; 3u |] "a lookahead at the very end"
            Expect.equal result.Props.Length result.Infos.Length "the properties still run beside the glyphs"
        }

        test "a ligature that formed after a failed ligature marks unsafe to concat from the glyph the ligation ended on" {
            let second = Digest.add 2u Digest.empty
            let ligatures = ImmutableArray.CreateRange [ { LigGlyph = 7u; Components = ImmutableArray.Create 9u }; { LigGlyph = 8u; Components = ImmutableArray.Create 2u } ]
            let table = tableOf [ accelOf 4 false (LigatureSubst (coverageOf 1u, ImmutableArray.Create ligatures, second)) ]
            let run = { runOf [| 1u; 2u; 3u |] with Flags = BufferFlags.ProduceUnsafeToConcat }
            let result = apply table [| 0 |] run
            Expect.equal (glyphIdsOf result) [| 8u; 3u |] "the second ligature forms"
            let flags = result.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask &&& uint32 GlyphFlags.UnsafeToConcat)
            Expect.equal flags [| 0u; 0u |] "LigatureSet::apply marks the range from the cursor after lig.apply, which is past the range, so nothing is marked (src/OT/Layout/GSUB/LigatureSet.hh:151-155)"
        }

        test "reverse chain refuses to run nested" {
            let reverse = ReverseChainSingle (coverageOf 1u, ImmutableArray.Empty, ImmutableArray.Empty, Sealed.ofSpan (ReadOnlySpan [| 50u |]))
            let context =
                SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 1u), ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }))
            let table = tableOf [ accelOf 8 true reverse; accelOf 5 false context ]
            let result = apply table [| 1 |] (runOf [| 1u; 2u |])
            Expect.equal (glyphIdsOf result) [| 1u; 2u |] "the reverse chain does not chain from a context lookup"
        }

        test "a reverse chain whose backtrack fails marks nothing: the lookahead is not read and the range ends at zero" {
            let reverse = ReverseChainSingle (coverageOf 1u, ImmutableArray.Create (coverageOf 9u), ImmutableArray.Create (coverageOf 1u), Sealed.ofSpan (ReadOnlySpan [| 50u |]))
            let table = tableOf [ accelOf 8 true reverse ]
            let run = { runOf [| 1u; 1u; 1u |] with Flags = BufferFlags.ProduceUnsafeToConcat }
            let result = apply table [| 0 |] run
            Expect.equal (glyphIdsOf result) [| 1u; 1u; 1u |] "no backtrack matches, so nothing is replaced"
            let flags = result.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask &&& uint32 GlyphFlags.UnsafeToConcat)
            Expect.equal flags [| 0u; 0u; 0u |] "unsafe_to_concat_from_outbuffer (start_index, 0) of src/OT/Layout/GSUB/ReverseChainSingleSubstFormat1.hh:233 marks nothing, end_index being left at its initial 0 (line 198)"
        }

        test "a class rule set whose digest refuses the first glyph flags the glyphs it looked at before it reads a skippable second glyph (hb-ot-layout-gsubgpos.hh:3398-3403)" {
            let classDef = ClassDefArray (1u, Sealed.ofList [ 1u; 2u; 0u; 0u ])
            let rule =
                { InputCount = 2; Input = Sealed.ofList [ 2u ]; Records = ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 } }
            let ruleSets = ImmutableArray.Create (ImmutableArray.Empty, ImmutableArray.CreateRange (List.replicate 5 rule))
            let firstClassDigests = Sealed.init 2 (fun index -> match index with | 1 -> 1UL <<< 2 | _ -> 0UL)
            let single = SingleList (coverageOf 1u, Sealed.ofSpan (ReadOnlySpan [| 30u |]))
            let context = SubstContext (ClassRules (coverageOf 1u, classDef, ruleSets, firstClassDigests))
            let table = tableOf [ accelOf 1 false single; accelOf 5 false context ]
            let joiner = { Unchecked.defaultof<GlyphProps> with UnicodeProps = UnicodeProps.Ignorable ||| UnicodeProps.CfZwj ||| 1us }
            let run = runOf [| 1u; 3u; 4u |]
            let run = { run with Flags = BufferFlags.ProduceUnsafeToConcat; Props = run.Props.SetItem(2, joiner) }
            let result = apply table [| 1 |] run
            let flags = result.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask &&& uint32 GlyphFlags.UnsafeToConcat)
            Expect.equal (glyphIdsOf result) [| 1u; 3u; 4u |] "no rule matches"
            Expect.equal flags [| 2u; 2u; 0u |] "the refused rule set flags the glyph and the first glyph after it, before the skippable second glyph is read"
        }
    ]
