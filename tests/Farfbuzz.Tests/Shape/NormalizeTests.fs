module Farfbuzz.Tests.Shape.NormalizeTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format 12 subtable.</summary>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The bytes.</returns>
let private format12 (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body = groups |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ]) |> Array.concat
    Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]

/// <summary>Builds a format 14 subtable with one non-default variation mapping.</summary>
/// <param name="selector">The variation selector.</param>
/// <param name="nonDefault">The code point and glyph.</param>
/// <returns>The bytes.</returns>
let private uvs14 (selector: uint32) (nonDefault: uint32 * uint32) : byte array =
    let headerSize = 21
    let defaultTable = Array.concat [ be32 1u; be24 0u; [| 0uy |] ]
    let codepoint, glyph = nonDefault
    let nonDefaultTable = Array.concat [ be32 1u; be24 codepoint; be16 glyph ]
    let record = Array.concat [ be24 selector; be32 (uint32 headerSize); be32 (uint32 (headerSize + defaultTable.Length)) ]
    let total = headerSize + defaultTable.Length + nonDefaultTable.Length
    Array.concat [ be16 14u; be32 (uint32 total); be32 1u; record; defaultTable; nonDefaultTable ]

/// <summary>Builds a cmap table from sorted encoding records.</summary>
/// <param name="records">Platform, encoding and subtable bytes.</param>
/// <returns>The cmap bytes.</returns>
let private cmapTable (records: (uint16 * uint16 * byte array) list) : byte array =
    let sorted = records |> List.sortBy (fun (platform, encoding, _) -> platform, encoding)
    let headerSize = 4 + 8 * sorted.Length
    let offsets =
        sorted
        |> List.fold (fun (offsets, offset) (_, _, bytes) -> (offset :: offsets, offset + bytes.Length)) ([], headerSize)
        |> fst
        |> List.rev
    let recordBytes =
        List.map2 (fun (platform, encoding, _) offset -> Array.concat [ be16 (uint32 platform); be16 (uint32 encoding); be32 (uint32 offset) ]) sorted offsets
        |> Array.concat
    let subtables = sorted |> List.map (fun (_, _, bytes) -> bytes) |> Array.concat
    Array.concat [ be16 0u; be16 (uint32 sorted.Length); recordBytes; subtables ]

/// <summary>Builds a font from one cmap table.</summary>
/// <param name="records">The encoding records.</param>
/// <returns>The font.</returns>
let private fontWith (records: (uint16 * uint16 * byte array) list) : Font =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmapTable records ])) 0u |> Font.create

/// <summary>Compiles the default plan for the normalize tests.</summary>
/// <returns>The plan.</returns>
let private defaultPlan () : OtShapePlan =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty DefaultShaper

/// <summary>Builds a run from code points and sets its Unicode properties.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
    |> OtProps.setUnicodeProps 0xFFFFFFFFu
    |> TailCalls.settle

/// <summary>Normalizes a run over a font.</summary>
/// <param name="font">The font.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The normalized run.</returns>
let private normalizeRun (font: Font) (codepoints: uint32 array) : GlyphRun =
    Normalize.run (defaultPlan ()) font (runOf codepoints)

/// <summary>Normalizes a run over a font with a custom not-found and invisible glyph.</summary>
/// <param name="font">The font.</param>
/// <param name="notFound">The not-found glyph.</param>
/// <param name="invisible">The invisible glyph.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The normalized run.</returns>
let private normalizeRunWithGlyphs (font: Font) (notFound: uint32) (invisible: uint32) (codepoints: uint32 array) : GlyphRun =
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
        |> GlyphRun.withNotFound notFound
        |> GlyphRun.withInvisible invisible
        |> OtProps.setUnicodeProps 0xFFFFFFFFu
        |> TailCalls.settle
    Normalize.run (defaultPlan ()) font run

/// <summary>Normalizes a run over a font with explicit clusters and a cluster level.</summary>
/// <param name="font">The font.</param>
/// <param name="level">The cluster level.</param>
/// <param name="clusters">The code points and their clusters.</param>
/// <returns>The normalized run.</returns>
let private normalizeRunClusters (font: Font) (level: ClusterLevel) (clusters: (uint32 * uint32) array) : GlyphRun =
    let run =
        clusters
        |> Array.fold (fun run (codepoint, cluster) -> GlyphRun.add codepoint cluster run) GlyphRun.empty
        |> GlyphRun.withClusterLevel level
        |> OtProps.setUnicodeProps 0xFFFFFFFFu
        |> TailCalls.settle
    Normalize.run (defaultPlan ()) font run

/// <summary>Reads the code points of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points.</returns>
let private codepointsOf (run: GlyphRun) : uint32 array =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)

/// <summary>Reads the glyph indices of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyph indices.</returns>
let private glyphsOf (run: GlyphRun) : uint32 array =
    run.Props.AsSpan().ToArray() |> Array.map (fun props -> props.GlyphIndex)

/// <summary>An edit of the model-property script, restricted to the new run-edit cases and their companions.</summary>
[<Struct>]
type private TestEdit =
    | ClearOut
    | NextOne
    | SetThenNext of GlyphProps
    | SetOut of int * uint32 * GlyphProps
    | PopOut

/// <summary>The state of the model-property replay pass.</summary>
[<Struct>]
type private TestReplayState =
    { Index: int
      PendingNext: bool }

/// <summary>A pass that replays a model-property edit script.</summary>
[<Struct>]
type private TestReplayPass =
    val Edits: TestEdit array
    /// <summary>Creates a replay pass.</summary>
    /// <param name="edits">The edit script.</param>
    new (edits) = { Edits = edits }
    interface IRunPass<TestReplayState> with
        member this.Step (_, state) =
            match state.PendingNext with
            | true -> struct (RunEdit.NextGlyph, { state with PendingNext = false })
            | false ->
                match state.Index >= this.Edits.Length with
                | true -> struct (RunEdit.Finish, state)
                | false ->
                    match this.Edits[state.Index] with
                    | ClearOut -> struct (RunEdit.ClearOutput, { Index = state.Index + 1; PendingNext = false })
                    | NextOne -> struct (RunEdit.NextGlyph, { Index = state.Index + 1; PendingNext = false })
                    | SetThenNext props -> struct (RunEdit.SetCurrentProps props, { Index = state.Index + 1; PendingNext = true })
                    | SetOut (index, codepoint, props) -> struct (RunEdit.SetOutputGlyph (index, codepoint, props), { Index = state.Index + 1; PendingNext = false })
                    | PopOut -> struct (RunEdit.PopOutput, { Index = state.Index + 1; PendingNext = false })

/// <summary>The list model of the out-buffer machine for the new edits.</summary>
type private TestModel =
    { Infos: GlyphInfo list
      Props: GlyphProps list
      OutInfos: GlyphInfo list
      OutProps: GlyphProps list
      Idx: int
      HaveOutput: bool }

/// <summary>Replaces one element of a list.</summary>
/// <param name="items">The list.</param>
/// <param name="index">The index.</param>
/// <param name="value">The value.</param>
/// <returns>The list with the value.</returns>
let private setAt (items: 'T list) (index: int) (value: 'T) : 'T list =
    items |> List.mapi (fun i item -> if i = index then value else item)

/// <summary>Drops the last element of a list.</summary>
/// <param name="items">The list.</param>
/// <returns>The list without its last element.</returns>
let private dropLast (items: 'T list) : 'T list =
    match items.Length with
    | 0 -> []
    | length -> List.take (length - 1) items

/// <summary>Applies one edit to the model.</summary>
/// <param name="model">The model.</param>
/// <param name="edit">The edit.</param>
/// <returns>The next model.</returns>
let rec private modelStep (model: TestModel) (edit: TestEdit) : TestModel =
    match edit with
    | ClearOut -> { model with OutInfos = []; OutProps = []; Idx = 0; HaveOutput = true }
    | NextOne ->
        match model.HaveOutput with
        | false -> { model with Idx = model.Idx + 1 }
        | true ->
            { model with
                OutInfos = model.OutInfos @ [ model.Infos[model.Idx] ]
                OutProps = model.OutProps @ [ model.Props[model.Idx] ]
                Idx = model.Idx + 1 }
    | SetThenNext props ->
        modelStep { model with Props = setAt model.Props model.Idx props } NextOne
    | SetOut (index, codepoint, props) ->
        { model with
            OutInfos = setAt model.OutInfos index { model.OutInfos[index] with Codepoint = codepoint }
            OutProps = setAt model.OutProps index props }
    | PopOut -> { model with OutInfos = dropLast model.OutInfos; OutProps = dropLast model.OutProps }

/// <summary>The model's final infos and props, after the sync the machine's <c>Finish</c> performs.</summary>
/// <param name="model">The model.</param>
/// <returns>The infos and props.</returns>
let private modelResult (model: TestModel) : struct (GlyphInfo array * GlyphProps array) =
    match model.HaveOutput with
    | false -> struct (Array.ofList model.Infos, Array.ofList model.Props)
    | true ->
        let remaining = List.skip model.Idx model.Infos
        let remainingProps = List.skip model.Idx model.Props
        struct (Array.ofList (model.OutInfos @ remaining), Array.ofList (model.OutProps @ remainingProps))

/// <summary>Applies an edit script to the model.</summary>
/// <param name="model">The model.</param>
/// <param name="edits">The edits.</param>
/// <returns>The final model.</returns>
let private modelStepChain (model: TestModel) (edits: TestEdit list) : TestModel =
    List.fold modelStep model edits

/// <summary>Generates random glyph properties.</summary>
/// <returns>The generator.</returns>
let private genProps : Gen<GlyphProps> =
    gen {
        let! unicode = Gen.choose (0, 0xFFFF) |> Gen.map uint16
        let! glyphClass = Gen.choose (0, 0xFFFF) |> Gen.map uint16
        let! ligProps = Gen.choose (0, 255) |> Gen.map byte
        let! syllable = Gen.choose (0, 255) |> Gen.map byte
        let! category = Gen.choose (0, 255) |> Gen.map byte
        let! auxiliary = Gen.choose (0, 255) |> Gen.map byte
        let! glyphIndex = Gen.choose (0, 1000) |> Gen.map uint32
        return
            { UnicodeProps = unicode
              GlyphClass = glyphClass
              LigProps = ligProps
              Syllable = syllable
              ShaperCategory = category
              ShaperAuxiliary = auxiliary
              GlyphIndex = glyphIndex }
    }

/// <summary>Generates one valid edit from the model.</summary>
/// <param name="model">The model.</param>
/// <returns>The edit generator.</returns>
let private genEdit (model: TestModel) : Gen<TestEdit> =
    let candidates = ResizeArray<Gen<TestEdit>>()
    match model.HaveOutput with
    | false -> candidates.Add(Gen.constant ClearOut)
    | true ->
        match model.Idx < model.Infos.Length with
        | true ->
            candidates.Add(Gen.constant NextOne)
            candidates.Add(gen { let! props = genProps in return SetThenNext props })
        | false -> ()
        match model.OutInfos.Length > 0 with
        | true ->
            candidates.Add(Gen.constant PopOut)
            candidates.Add(gen {
                let! index = Gen.choose (0, model.OutInfos.Length - 1)
                let! codepoint = Gen.choose (0, 200) |> Gen.map uint32
                let! props = genProps
                return SetOut (index, codepoint, props)
            })
        | false -> ()
    Gen.oneof (List.ofSeq candidates)

/// <summary>Whether the model can still produce an edit.</summary>
/// <param name="model">The model.</param>
/// <returns>True while a valid edit remains.</returns>
let private canEdit (model: TestModel) : bool =
    match model.HaveOutput with
    | false -> true
    | true -> model.Idx < model.Infos.Length || model.OutInfos.Length > 0

/// <summary>Generates a model and an edit script.</summary>
/// <param name="model">The model.</param>
/// <param name="remaining">How many edits remain.</param>
/// <returns>The edit-script generator.</returns>
let rec private genScript (model: TestModel) (remaining: int) : Gen<TestEdit array> =
    gen {
        match remaining <= 0 || not (canEdit model) with
        | true -> return [||]
        | false ->
            let! edit = genEdit model
            let next = modelStep model edit
            let! rest = genScript next (remaining - 1)
            return Array.append [| edit |] rest
    }

/// <summary>Builds the machine input run for a model.</summary>
/// <param name="model">The model.</param>
/// <returns>The run.</returns>
let private modelRun (model: TestModel) : GlyphRun =
    { GlyphRun.empty with
        Infos = Sealed.ofSpan (ReadOnlySpan (Array.ofList model.Infos))
        Props = Sealed.ofSpan (ReadOnlySpan (Array.ofList model.Props))
        Successful = true }

/// <summary>The model-property input: a model and its edit script.</summary>
let private modelInput : Gen<TestModel * TestEdit array> =
    gen {
        let! length = Gen.choose (1, 6)
        let! codepoints = Gen.listOfLength length (Gen.choose (0, 200) |> Gen.map uint32)
        let! props = Gen.listOfLength length genProps
        let model =
            { Infos = codepoints |> List.mapi (fun i codepoint -> { Codepoint = codepoint; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
              Props = props
              OutInfos = []
              OutProps = []
              Idx = 0
              HaveOutput = false }
        let! editCount = Gen.choose (1, 10)
        let! edits = genScript model editCount
        return model, edits
    }

/// <summary>The P4.S03 normalizer tests.</summary>
[<Tests>]
let tests =
    testList "P4.S03" [
        testProperty "the new run edits match a list model of info and out_info" (
            Prop.forAll (Arb.fromGen modelInput) (fun (model, edits) ->
                let run = modelRun model
                let shaped = GlyphRun.runPass (TestReplayPass(edits)) { Index = 0; PendingNext = false } run
                let final = modelStepChain model (List.ofArray edits)
                let struct (expectedInfos, expectedProps) = modelResult final
                let actualInfos = shaped.Infos.AsSpan().ToArray()
                let actualProps = shaped.Props.AsSpan().ToArray()
                let ok = actualInfos = expectedInfos && actualProps = expectedProps
                match ok with
                | true -> Prop.ofTestable true
                | false -> Prop.ofTestable false |> Prop.label $"edits: %A{edits}\n  model:  %A{expectedInfos} %A{expectedProps}\n  actual: %A{actualInfos} %A{actualProps}"))
        test "é keeps its glyph when present, else decomposes to e and the acute" {
            let present = fontWith [ 3us, 10us, format12 [ 0xE9u, 0xE9u, 1u ] ]
            let absent = fontWith [ 3us, 10us, format12 [ 0x65u, 0x65u, 2u; 0x301u, 0x301u, 3u ] ]
            let kept = normalizeRun present [| 0xE9u |]
            let decomposed = normalizeRun absent [| 0xE9u |]
            Expect.equal (codepointsOf kept) [| 0xE9u |] "kept codepoint"
            Expect.equal (glyphsOf kept) [| 1u |] "kept glyph"
            Expect.equal (codepointsOf decomposed) [| 0x65u; 0x301u |] "decomposed codepoints"
            Expect.equal (glyphsOf decomposed) [| 2u; 3u |] "decomposed glyphs"
        }
        test "a decomposable glyph after another simple glyph keeps its own glyph" {
            let font = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 4u; 0x65u, 0x65u, 2u; 0xE9u, 0xE9u, 1u; 0x301u, 0x301u, 3u ] ]
            let run = normalizeRun font [| 0x61u; 0xE9u |]
            Expect.equal (codepointsOf run) [| 0x61u; 0xE9u |] "no decomposition"
            Expect.equal (glyphsOf run) [| 4u; 1u |] "each keeps its glyph"
        }
        test "marks reorder by combining class" {
            let font = fontWith [ 3us, 10us, format12 [ 0x65u, 0x65u, 1u; 0x301u, 0x301u, 2u; 0x327u, 0x327u, 3u ] ]
            let run = normalizeRun font [| 0x65u; 0x301u; 0x327u |]
            Expect.equal (codepointsOf run) [| 0x65u; 0x327u; 0x301u |] "cedilla before acute"
        }
        test "an LVT syllable decomposes to LV and T" {
            let font = fontWith [ 3us, 10us, format12 [ 0x11A8u, 0x11A8u, 2u; 0xAC00u, 0xAC00u, 1u ] ]
            let run = normalizeRun font [| 0xAC01u |]
            Expect.equal (codepointsOf run) [| 0xAC00u; 0x11A8u |] "LV + T"
            Expect.equal (glyphsOf run) [| 1u; 2u |] "glyphs"
        }
        test "a glyphless en space falls back to the space glyph" {
            let font = fontWith [ 3us, 10us, format12 [ 0x20u, 0x20u, 1u ] ]
            let run = normalizeRun font [| 0x2002u |]
            Expect.equal (codepointsOf run) [| 0x2002u |] "codepoint kept"
            Expect.equal (glyphsOf run) [| 1u |] "space glyph"
            Expect.equal (UnicodeProps.spaceFallback run.Props[0]) SpaceFallback.SpaceEm2 "space type"
            Expect.isTrue ((run.ScratchFlags &&& ScratchFlags.HasSpaceFallback) = ScratchFlags.HasSpaceFallback) "space fallback flag"
        }
        test "U+2011 takes the hyphen's glyph" {
            let font = fontWith [ 3us, 10us, format12 [ 0x2010u, 0x2010u, 1u ] ]
            let run = normalizeRun font [| 0x2011u |]
            Expect.equal (codepointsOf run) [| 0x2011u |] "codepoint kept"
            Expect.equal (glyphsOf run) [| 1u |] "hyphen glyph"
        }
        test "a variation selector hit collapses the pair and a miss passes both" {
            let hit = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 1u ]; 0us, 5us, uvs14 0xFE00u (0x61u, 5u) ]
            let miss = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 1u ] ]
            let hitRun = normalizeRun hit [| 0x61u; 0xFE00u |]
            let missRun = normalizeRun miss [| 0x61u; 0xFE00u |]
            Expect.equal (codepointsOf hitRun) [| 0x61u |] "hit collapses"
            Expect.equal (glyphsOf hitRun) [| 5u |] "hit glyph"
            Expect.equal (codepointsOf missRun) [| 0x61u; 0xFE00u |] "miss keeps both"
            Expect.equal (glyphsOf missRun) [| 1u; 0u |] "miss glyphs"
            Expect.isTrue ((missRun.ScratchFlags &&& ScratchFlags.HasVariationSelectorFallback) = ScratchFlags.HasVariationSelectorFallback) "miss flag"
        }
        test "a passive combining grapheme joiner is unhidden" {
            let font = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 1u; 0x301u, 0x301u, 2u ] ]
            let run = normalizeRun font [| 0x61u; 0x34Fu; 0x301u |]
            Expect.isFalse (UnicodeProps.isHidden run.Props[1]) "CGJ unhidden"
        }
        test "a run of 34 combining marks is not sorted" {
            let marks = Array.concat [| [| 0x301u |]; Array.create 33 0x327u |]
            let font = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 1u; 0x301u, 0x301u, 2u; 0x327u, 0x327u, 3u ] ]
            let run = normalizeRun font (Array.concat [| [| 0x61u |]; marks |])
            Expect.equal run.Infos[1].Codepoint 0x301u "acute stays first"
            Expect.equal run.Infos[2].Codepoint 0x327u "cedilla stays second"
        }
        test "a glyphless space without a space glyph falls back to the not-found glyph when invisible is zero" {
            let font = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 9u ] ]
            let run = normalizeRunWithGlyphs font 5u 0u [| 0x2002u |]
            Expect.equal (glyphsOf run) [| 5u |] "not-found glyph"
            Expect.equal (UnicodeProps.spaceFallback run.Props[0]) SpaceFallback.NotSpace "space type"
            Expect.equal (run.ScratchFlags &&& ScratchFlags.HasSpaceFallback) ScratchFlags.Default "no space fallback flag"
        }
        test "a glyphless space without a space glyph uses the invisible glyph when set" {
            let font = fontWith [ 3us, 10us, format12 [ 0x61u, 0x61u, 9u ] ]
            let run = normalizeRunWithGlyphs font 5u 7u [| 0x2002u |]
            Expect.equal (glyphsOf run) [| 7u |] "invisible glyph"
            Expect.equal (UnicodeProps.spaceFallback run.Props[0]) SpaceFallback.SpaceEm2 "space type"
            Expect.isTrue ((run.ScratchFlags &&& ScratchFlags.HasSpaceFallback) = ScratchFlags.HasSpaceFallback) "space fallback flag"
        }
        test "e and the acute with clusters 5 and 3 compose to one glyph with the lower cluster" {
            let font = fontWith [ 3us, 10us, format12 [ 0x65u, 0x65u, 1u; 0xE9u, 0xE9u, 3u; 0x301u, 0x301u, 2u ] ]
            let run = normalizeRunClusters font ClusterLevel.MonotoneGraphemes [| (0x65u, 5u); (0x301u, 3u) |]
            Expect.equal (codepointsOf run) [| 0xE9u |] "composed"
            Expect.equal run.Infos[0].Cluster 3u "cluster is the lower one"
        }
        test "the composed starter keeps the merge's cleared glyph flags at MonotoneCharacters" {
            let font = fontWith [ 3us, 10us, format12 [ 0x65u, 0x65u, 1u; 0xE9u, 0xE9u, 3u; 0x301u, 0x301u, 2u ] ]
            let run = normalizeRunClusters font ClusterLevel.MonotoneCharacters [| (0x65u, 5u); (0x301u, 3u) |]
            Expect.equal (codepointsOf run) [| 0xE9u |] "composed"
            Expect.equal (run.Infos[0].Mask &&& 7u) 0u "the merge clears the glyph flags"
        }
    ]
