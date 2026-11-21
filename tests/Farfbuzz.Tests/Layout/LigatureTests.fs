module Farfbuzz.Tests.Layout.LigatureTests

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Expecto
open Farfbuzz

/// <summary>An empty GDEF for the ligature tests.</summary>
let private emptyGdef =
    { Version = 0u
      GlyphClasses = ClassDefNull
      MarkAttachClasses = ClassDefNull
      MarkGlyphSets = ImmutableArray.Empty
      LigCarets = ReadOnlyMemory.Empty
      AttachList = ReadOnlyMemory.Empty
      HasVarStore = false
      HasGlyphClasses = false
      LigCaretsWide = false
      GlyphProps = ImmutableArray.Empty }

/// <summary>Builds a glyph info with a code point and mask.</summary>
/// <param name="codepoint">The code point.</param>
/// <param name="mask">The mask.</param>
/// <returns>The glyph info.</returns>
let private infoOf (codepoint: uint32) (mask: uint32) =
    { Codepoint = codepoint; Mask = mask; Cluster = 0u; Var1 = 0u; Var2 = 0u }

/// <summary>Builds empty glyph properties.</summary>
/// <returns>The properties.</returns>
let private propsOf () =
    { UnicodeProps = 0us
      GlyphClass = 0us
      LigProps = 0uy
      Syllable = 0uy
      ShaperCategory = 0uy
      ShaperAuxiliary = 0uy
      GlyphIndex = 0u }

/// <summary>Builds properties with a glyph class and lig props.</summary>
/// <param name="glyphClass">The glyph class.</param>
/// <param name="ligProps">The lig props.</param>
/// <returns>The properties.</returns>
let private propsWith (glyphClass: uint16) (ligProps: byte) =
    { UnicodeProps = 0us
      GlyphClass = glyphClass
      LigProps = ligProps
      Syllable = 0uy
      ShaperCategory = 0uy
      ShaperAuxiliary = 0uy
      GlyphIndex = 0u }

/// <summary>Builds a ligature.</summary>
/// <param name="ligGlyph">The ligature glyph.</param>
/// <param name="components">The components after the first.</param>
/// <returns>The ligature.</returns>
let private ligOf (ligGlyph: uint32) (components: uint32 list) =
    { LigGlyph = ligGlyph; Components = Sealed.ofList components }

/// <summary>Builds a single-glyph coverage.</summary>
/// <param name="glyph">The glyph.</param>
/// <returns>The coverage.</returns>
let private coverageOf (glyph: uint32) =
    CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| glyph |]))

/// <summary>Folds one ligature set's second glyphs into a digest, full for a ligature without components.</summary>
/// <param name="ligs">The ligatures.</param>
/// <param name="index">The next ligature.</param>
/// <param name="digest">The digest so far.</param>
/// <returns>The digest.</returns>
[<TailCall>]
let rec private foldLigSeconds (ligs: ImmutableArray<Ligature>) (index: int) (digest: Digest) =
    match index < ligs.Length with
    | false -> digest
    | true ->
        let next =
            match ligs[index].Components.IsDefault || ligs[index].Components.Length = 0 with
            | true -> Digest.full
            | false -> Digest.add ligs[index].Components[0] digest
        foldLigSeconds ligs (index + 1) next

/// <summary>Folds every ligature set's second glyphs into a digest.</summary>
/// <param name="sets">The ligature sets.</param>
/// <param name="index">The next set.</param>
/// <param name="digest">The digest so far.</param>
/// <returns>The digest.</returns>
[<TailCall>]
let rec private foldSetSeconds (sets: ImmutableArray<ImmutableArray<Ligature>>) (index: int) (digest: Digest) =
    match index < sets.Length with
    | false -> digest
    | true -> foldSetSeconds sets (index + 1) (foldLigSeconds sets[index] 0 digest)

/// <summary>Folds a ligature subtable's second glyphs into a digest.</summary>
/// <param name="sets">The ligature sets.</param>
/// <returns>The digest.</returns>
let private ligSecondDigest (sets: ImmutableArray<ImmutableArray<Ligature>>) =
    foldSetSeconds sets 0 Digest.empty

/// <summary>Builds a GSUB table of one ligature lookup over a coverage.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="sets">The ligature sets.</param>
/// <returns>The table.</returns>
let private ligatureTableOf (coverage: Coverage) (sets: ImmutableArray<ImmutableArray<Ligature>>) =
    let secondDigest = ligSecondDigest sets
    let subtable = LigatureSubst (coverage, sets, secondDigest)
    let accel = { Subtable = subtable; SubtableDigest = Coverage.digest coverage }
    let lookup =
        { LookupType = 4
          Props = 0u
          IsReverse = false
          Subtables = ImmutableArray.Create accel
          LookupDigest = Coverage.digest coverage
          SecondDigest = secondDigest }
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy lookup)
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>A lookup map with a non-zero mask.</summary>
/// <returns>The lookup map.</returns>
let private lookupMapOf () =
    { LookupIndex = 0
      AutoZwnj = false
      AutoZwj = false
      Random = false
      PerSyllable = false
      Mask = 1u
      FeatureTag = Tag.make 'c' 'c' 'm' 'p' }

/// <summary>One stage holding the single lookup.</summary>
/// <returns>The stages.</returns>
let private stageOf () = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }

/// <summary>Applies one ligature lookup to a run.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="sets">The ligature sets.</param>
/// <param name="run">The run.</param>
/// <returns>The substituted run.</returns>
let private applyLigature (coverage: Coverage) (sets: ImmutableArray<ImmutableArray<Ligature>>) (run: GlyphRun) =
    let table = ligatureTableOf coverage sets
    Engine.applyTable false (Engine.GsubApplier()) table (ImmutableArray.Create (lookupMapOf ())) (stageOf ()) (Font.create Face.empty) run

/// <summary>Builds a run of glyphs with the given properties.</summary>
/// <param name="codepoints">The code points.</param>
/// <param name="props">The properties.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 list) (props: GlyphProps list) =
    let infos = Sealed.init codepoints.Length (fun i -> infoOf codepoints[i] 1u)
    let propsArray = Sealed.ofList props
    { GlyphRun.empty with Infos = infos; Props = propsArray }

/// <summary>Measures the bytes one call allocates, after a warm-up call.</summary>
/// <param name="action">The call to measure.</param>
/// <returns>The bytes allocated by the measured call.</returns>
let private measureAlloc (action: unit -> unit) : int64 =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    action ()
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>Builds a run of glyphs with a non-zero mask.</summary>
/// <param name="count">The glyph count.</param>
/// <returns>The run.</returns>
let private maskedRunOf (count: int) =
    let infos = Sealed.init count (fun i -> infoOf (uint32 (i % 128)) 1u)
    let props = Sealed.init count (fun _ -> propsOf ())
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>The P5.S08 GSUB ligature cases.</summary>
[<Tests>]
let tests =
    testList "P5.S08" [
        test "a two-glyph ligature allocates a ligature id and component count" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let run = runOf [ 0x61u; 0x62u ] [ propsOf (); propsOf () ]
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 100u |] "the two glyphs ligate"
            Expect.isTrue (GlyphPropsBits.isLigature shaped.Props[0]) "the ligature is classified as a ligature"
            Expect.notEqual (GlyphPropsBits.ligId shaped.Props[0]) 0uy "the ligature allocates a ligature id"
            Expect.equal (GlyphPropsBits.ligNumComps shaped.Props[0]) 2uy "the ligature carries two components"
        }
        test "a mark ligature keeps its previous ligature id" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let first = propsWith GlyphPropsBits.Mark 0x61uy
            let second = propsWith GlyphPropsBits.Mark 0x61uy
            let run = runOf [ 0x61u; 0x62u ] [ first; second ]
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 100u |] "the marks ligate"
            Expect.isFalse (GlyphPropsBits.isLigature shaped.Props[0]) "a mark ligature is not classified as a ligature"
            Expect.equal (GlyphPropsBits.ligId shaped.Props[0]) 3uy "the mark ligature keeps its old ligature id"
        }
        test "a base with marks ligature stays a base" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let first = propsWith GlyphPropsBits.Base 0uy
            let second = propsWith GlyphPropsBits.Mark 0uy
            let run = runOf [ 0x61u; 0x62u ] [ first; second ]
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 100u |] "the base and mark ligate"
            Expect.isFalse (GlyphPropsBits.isLigature shaped.Props[0]) "a base ligature is not classified as a ligature"
            Expect.equal (GlyphPropsBits.ligId shaped.Props[0]) 0uy "a base ligature allocates no ligature id"
        }
        test "a one-component ligature is a plain substitution" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u []))
            let run = runOf [ 0x61u ] [ propsOf () ]
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 100u |] "the glyph is replaced"
            Expect.isFalse (GlyphPropsBits.isLigature shaped.Props[0]) "a one-component ligature is not ligated"
        }
        test "re-adjust of following marks reassigns the ligature component" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let first = propsOf ()
            let nested = propsWith GlyphPropsBits.Ligature 0x72uy
            let mark = propsWith GlyphPropsBits.Mark 0x62uy
            let run = runOf [ 0x61u; 0x62u; 0x63u ] [ first; nested; mark ]
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 100u; 0x63u |] "the nested ligature and its mark survive"
            Expect.equal (GlyphPropsBits.ligId shaped.Props[0]) (GlyphPropsBits.ligId shaped.Props[1]) "the mark joins the new ligature"
            Expect.equal (GlyphPropsBits.ligComp shaped.Props[1]) 3uy "the mark is reassigned to component three"
        }
        test "component counts accumulate through a nested ligature" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let first = propsOf ()
            let nested = propsWith GlyphPropsBits.Ligature 0x72uy
            let run = runOf [ 0x61u; 0x62u ] [ first; nested ]
            let shaped = applyLigature coverage sets run
            Expect.equal (GlyphPropsBits.ligNumComps shaped.Props[0]) 3uy "the nested ligature contributes its two components"
        }
        test "ligature ids wrap after 255 serials" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let codepoints = List.init 16 (fun i -> if i % 2 = 0 then 0x61u else 0x62u)
            let run = { runOf codepoints (List.init 16 (fun _ -> propsOf ())) with Serial = 255u }
            let shaped = applyLigature coverage sets run
            let ligIds = shaped.Props.AsSpan().ToArray() |> Array.map (fun p -> GlyphPropsBits.ligId p)
            Expect.equal ligIds [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 1uy |] "the eighth ligature wraps to id one"
        }
        test "the fast path marks no unsafe-to-concat once a ligature after a skipped one is formed" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x63u ], ligOf 101u [ 0x62u ]))
            let run = { runOf [ 0x61u; 0x62u; 0x63u ] [ propsOf (); propsOf (); propsOf () ] with Flags = BufferFlags.ProduceUnsafeToConcat }
            let shaped = applyLigature coverage sets run
            Expect.equal (shaped.Infos.AsSpan().ToArray() |> Array.map (fun i -> i.Codepoint)) [| 101u; 0x63u |] "the second ligature matches"
            Expect.equal (shaped.Infos[0].Mask &&& 2u) 0u "LigatureSet::apply marks the range from the cursor after lig.apply, which is past the range (src/OT/Layout/GSUB/LigatureSet.hh:151-155): HarfBuzz marks nothing"
        }
        test "a non-matching ligature lookup allocates nothing" {
            let table = ligatureTableOf (coverageOf 0xFFFFu) (ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ])))
            let lookups = ImmutableArray.Create (lookupMapOf ())
            let stages = stageOf ()
            let font = Font.create Face.empty
            let run = maskedRunOf 1000
            let apply () = Engine.applyTable false (Engine.GsubApplier()) table lookups stages font run |> ignore
            Expect.equal (measureAlloc apply) 0L "a non-matching ligature lookup allocates nothing"
        }
        test "a ligature allocates only the output arrays" {
            let coverage = CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })
            let table = ligatureTableOf coverage (ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 1u ])))
            let lookups = ImmutableArray.Create (lookupMapOf ())
            let stages = stageOf ()
            let font = Font.create Face.empty
            let infos = Sealed.init 1000 (fun i -> infoOf (if i % 2 = 0 then 0u else 1u) 1u)
            let props = Sealed.init 1000 (fun _ -> propsOf ())
            let run = { GlyphRun.empty with Infos = infos; Props = props }
            let apply () = Engine.applyTable false (Engine.GsubApplier()) table lookups stages font run |> ignore
            let allocated = measureAlloc apply
            let outputBytes = int64 (1000 * 4 * (Unsafe.SizeOf<GlyphInfo>() + Unsafe.SizeOf<GlyphProps>())) + 8192L
            Expect.isLessThanOrEqual allocated outputBytes $"a ligature allocates at most the output arrays (allocated {allocated})"
        }
        test "ligating keeps the properties parallel to the glyphs" {
            let coverage = coverageOf 0x61u
            let sets = ImmutableArray.Create (ImmutableArray.Create (ligOf 100u [ 0x62u ]))
            let run = runOf [ 0x61u; 0x62u; 0x61u; 0x62u ] [ propsOf (); propsOf (); propsOf (); propsOf () ]
            let shaped = applyLigature coverage sets run
            Expect.equal shaped.Props.Length shaped.Infos.Length "every glyph keeps its properties"
        }
    ]
