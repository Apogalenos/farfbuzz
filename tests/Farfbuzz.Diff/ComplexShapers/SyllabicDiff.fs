module Farfbuzz.Diff.ComplexShapers.SyllabicDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>The dotted-circle insertion parameters of one shaper.</summary>
type private Params =
    { BrokenType: byte
      Category: byte
      Repha: byte voption
      Position: byte voption }

/// <summary>The union of the indic, khmer and myanmar machine exports and every value the indic category table yields.</summary>
let private a35 : byte array =
    [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy; 11uy; 12uy; 13uy; 14uy; 15uy; 16uy; 17uy; 18uy
       20uy; 21uy; 22uy; 23uy; 25uy; 26uy; 27uy; 32uy
       35uy; 36uy; 37uy; 38uy; 39uy; 40uy; 41uy; 57uy |]

/// <summary>The 44 USE category exports.</summary>
let private useExports : byte array =
    [| 0uy; 1uy; 4uy; 5uy; 6uy; 11uy; 12uy; 13uy; 14uy; 16uy; 18uy
       22uy; 23uy; 24uy; 25uy; 26uy; 27uy; 28uy; 29uy; 30uy; 31uy; 32uy; 33uy; 34uy; 35uy
       37uy; 38uy; 39uy; 41uy; 42uy; 43uy; 44uy; 45uy; 46uy; 47uy; 48uy; 49uy; 50uy; 51uy; 52uy; 53uy; 54uy; 55uy; 56uy |]

/// <summary>Finds a test font by its file name.</summary>
/// <param name="name">The file name.</param>
/// <returns>The test font.</returns>
let private fontByName (name: string) : Farfbuzz.Diff.TestFonts.TestFont =
    Farfbuzz.Diff.TestFonts.files ()
    |> List.find (fun (_, path) -> Path.GetFileName path = name)
    |> fun (suite, path) -> { Path = path; Suite = suite; Index = 0 }

/// <summary>The Indic font that carries U+25CC.</summary>
let private dottedCircleFont = fontByName "1a5face3fcbd929d228235c2f72bbd6f8eb37424.ttf"

/// <summary>A font that does not carry U+25CC.</summary>
let private noDottedCircleFont = fontByName "49c9f7485c1392fa09a1b801bc2ffea79275f22e.ttf"

/// <summary>The output of the dotted-circle pass: the glyphs, clusters and the three shaper lanes.</summary>
type private Output =
    { Glyphs: uint32 array
      Clusters: uint32 array
      Syllables: byte array
      Categories: byte array
      Auxiliary: byte array }

/// <summary>Finds the syllables of a category run and builds its glyph and cluster arrays.</summary>
/// <param name="machine">The machine.</param>
/// <param name="categories">The category bytes.</param>
/// <returns>The glyphs, clusters and syllable bytes.</returns>
let private buildInput (machine: RagelMachine) (categories: byte array) : struct (uint32 array * uint32 array * byte array) =
    let length = categories.Length
    let cat = Sealed.ofSpan (ReadOnlySpan categories)
    let previous = Sealed.init length (fun _ -> 0uy)
    let struct (syllables, _) = Ragel.findSyllables machine cat (Unchecked.defaultof<ImmutableArray<int>>) previous
    let glyphs = Array.init length (fun i -> uint32 (i + 1))
    let clusters = Array.init length uint32
    struct (glyphs, clusters, syllables.AsSpan().ToArray())

/// <summary>Reads the output of the Farfbuzz dotted-circle pass.</summary>
/// <param name="run">The run.</param>
/// <returns>The output.</returns>
let private extract (run: GlyphRun) : Output =
    let length = run.Infos.Length
    { Glyphs = Array.init length (fun i -> run.Infos[i].Codepoint)
      Clusters = Array.init length (fun i -> run.Infos[i].Cluster)
      Syllables = Array.init length (fun i -> run.Props[i].Syllable)
      Categories = Array.init length (fun i -> run.Props[i].ShaperCategory)
      Auxiliary = Array.init length (fun i -> run.Props[i].ShaperAuxiliary) }

/// <summary>Farfbuzz's dotted-circle insertion over a category run.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="machine">The machine.</param>
/// <param name="ps">The parameters.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="categories">The category bytes.</param>
/// <returns>The output.</returns>
let private ours (font: Font) (machine: RagelMachine) (ps: Params) (flags: BufferFlags) (categories: byte array) : Output =
    let struct (glyphs, clusters, syllables) = buildInput machine categories
    let length = categories.Length
    let infos =
        Sealed.init length (fun i ->
            { Codepoint = glyphs[i]; Mask = 0u; Cluster = clusters[i]; Var1 = 0u; Var2 = 0u })
    let props =
        Sealed.init length (fun i ->
            { UnicodeProps = 0us
              GlyphClass = 0us
              LigProps = 0uy
              Syllable = syllables[i]
              ShaperCategory = categories[i]
              ShaperAuxiliary = 0x7Fuy
              GlyphIndex = 0u })
    let run =
        { GlyphRun.empty with
            Infos = infos
            Props = props
            Flags = flags
            ScratchFlags = ScratchFlags.HasBrokenSyllable }
    let struct (result, _) =
        SyllableEdit.insertDottedCircles font ps.BrokenType ps.Category ps.Repha ps.Position run
    extract result

/// <summary>The oracle's dotted-circle insertion over a category run.</summary>
/// <param name="fontHandle">The oracle font handle.</param>
/// <param name="machine">The machine.</param>
/// <param name="ps">The parameters.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="categories">The category bytes.</param>
/// <returns>The output.</returns>
let private oracle (fontHandle: nativeint) (machine: RagelMachine) (ps: Params) (flags: BufferFlags) (categories: byte array) : Output =
    let struct (glyphs, clusters, syllables) = buildInput machine categories
    let length = categories.Length
    let capacity = 2u * uint32 length
    let outGlyphs = Array.zeroCreate<uint32> (int capacity)
    let outClusters = Array.zeroCreate<uint32> (int capacity)
    let outSyllables = Array.zeroCreate<byte> (int capacity)
    let outCategories = Array.zeroCreate<byte> (int capacity)
    let outAuxiliary = Array.zeroCreate<byte> (int capacity)
    let repha = match ps.Repha with | ValueSome r -> int r | ValueNone -> -1
    let position = match ps.Position with | ValueSome v -> int v | ValueNone -> -1
    let written =
        Shims.farfbuzz_oracle_insert_dotted_circles
            ( fontHandle
            , uint32 flags
            , glyphs
            , clusters
            , syllables
            , categories
            , uint32 length
            , uint32 ps.BrokenType
            , uint32 ps.Category
            , repha
            , position
            , outGlyphs
            , outClusters
            , outSyllables
            , outCategories
            , outAuxiliary
            , capacity )
    let n = int written
    { Glyphs = Array.truncate n outGlyphs
      Clusters = Array.truncate n outClusters
      Syllables = Array.truncate n outSyllables
      Categories = Array.truncate n outCategories
      Auxiliary = Array.truncate n outAuxiliary }

/// <summary>A random category run over an alphabet.</summary>
/// <param name="alphabet">The category alphabet.</param>
/// <returns>The generator.</returns>
let private sampledCategories (alphabet: byte array) : Gen<byte array> =
    gen {
        let! length = Gen.choose (0, 32)
        return! Gen.arrayOfLength length (Gen.elements alphabet)
    }

/// <summary>The four shaper parameter sets.</summary>
let private indicParams : Params =
    { BrokenType = 4uy
      Category = IndicMachine.DOTTEDCIRCLE
      Repha = ValueSome IndicMachine.Repha
      Position = ValueSome IndicPosition.PosEnd }

/// <summary>The Khmer parameters.</summary>
let private khmerParams : Params =
    { BrokenType = 1uy
      Category = KhmerMachine.DOTTEDCIRCLE
      Repha = ValueNone
      Position = ValueNone }

/// <summary>The Myanmar parameters.</summary>
let private myanmarParams : Params =
    { BrokenType = 1uy
      Category = MyanmarMachine.DOTTEDCIRCLE
      Repha = ValueNone
      Position = ValueNone }

/// <summary>The USE parameters.</summary>
let private useParams : Params =
    { BrokenType = 7uy
      Category = UseMachine.B
      Repha = ValueSome UseMachine.R
      Position = ValueNone }

/// <summary>The P6.S10 dotted-circle differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let font = Font.create (Face.create (ReadOnlyMemory (Farfbuzz.Diff.OracleFaces.data dottedCircleFont.Path)) 0u)
    let fontHandle = Farfbuzz.Diff.OracleFaces.font dottedCircleFont 0u
    let noDottedCircle = Font.create (Face.create (ReadOnlyMemory (Farfbuzz.Diff.OracleFaces.data noDottedCircleFont.Path)) 0u)
    testList "P6.S10" [
        sampled "indic dotted circles" (sampledCategories a35) 20_000
            (fun categories -> ours font IndicMachine.machine indicParams BufferFlags.Default categories)
            (fun categories -> oracle fontHandle IndicMachine.machine indicParams BufferFlags.Default categories)
        sampled "khmer dotted circles" (sampledCategories a35) 20_000
            (fun categories -> ours font KhmerMachine.machine khmerParams BufferFlags.Default categories)
            (fun categories -> oracle fontHandle KhmerMachine.machine khmerParams BufferFlags.Default categories)
        sampled "myanmar dotted circles" (sampledCategories a35) 20_000
            (fun categories -> ours font MyanmarMachine.machine myanmarParams BufferFlags.Default categories)
            (fun categories -> oracle fontHandle MyanmarMachine.machine myanmarParams BufferFlags.Default categories)
        sampled "use dotted circles" (sampledCategories useExports) 20_000
            (fun categories -> ours font UseMachine.machine useParams BufferFlags.Default categories)
            (fun categories -> oracle fontHandle UseMachine.machine useParams BufferFlags.Default categories)
        testCase "a font without U+25CC inserts nothing" (fun () ->
            Expect.equal (Font.nominalGlyph noDottedCircle 0x25CCu) ValueNone "the font has no dotted circle"
            for machine, ps in [ IndicMachine.machine, indicParams; KhmerMachine.machine, khmerParams; MyanmarMachine.machine, myanmarParams; UseMachine.machine, useParams ] do
                let categories = [| 4uy; 4uy; 4uy |]
                let struct (run, ran) =
                    let struct (glyphs, clusters, syllables) = buildInput machine categories
                    let infos = Sealed.init categories.Length (fun i -> { Codepoint = glyphs[i]; Mask = 0u; Cluster = clusters[i]; Var1 = 0u; Var2 = 0u })
                    let props = Sealed.init categories.Length (fun i -> { UnicodeProps = 0us; GlyphClass = 0us; LigProps = 0uy; Syllable = syllables[i]; ShaperCategory = categories[i]; ShaperAuxiliary = 0x7Fuy; GlyphIndex = 0u })
                    let run = { GlyphRun.empty with Infos = infos; Props = props; ScratchFlags = ScratchFlags.HasBrokenSyllable }
                    SyllableEdit.insertDottedCircles noDottedCircle ps.BrokenType ps.Category ps.Repha ps.Position run
                Expect.isFalse ran "no dotted circle, so the pass did not run"
                Expect.equal run.Infos.Length categories.Length "the run is unchanged")
        testCase "do not insert dotted circle is a no-op" (fun () ->
            for machine, ps in [ IndicMachine.machine, indicParams; KhmerMachine.machine, khmerParams; MyanmarMachine.machine, myanmarParams; UseMachine.machine, useParams ] do
                let categories = [| 4uy; 4uy; 4uy |]
                let struct (run, ran) =
                    let struct (glyphs, clusters, syllables) = buildInput machine categories
                    let infos = Sealed.init categories.Length (fun i -> { Codepoint = glyphs[i]; Mask = 0u; Cluster = clusters[i]; Var1 = 0u; Var2 = 0u })
                    let props = Sealed.init categories.Length (fun i -> { UnicodeProps = 0us; GlyphClass = 0us; LigProps = 0uy; Syllable = syllables[i]; ShaperCategory = categories[i]; ShaperAuxiliary = 0x7Fuy; GlyphIndex = 0u })
                    let run = { GlyphRun.empty with Infos = infos; Props = props; Flags = BufferFlags.DoNotInsertDottedCircle; ScratchFlags = ScratchFlags.HasBrokenSyllable }
                    SyllableEdit.insertDottedCircles font ps.BrokenType ps.Category ps.Repha ps.Position run
                Expect.isFalse ran "the flag suppressed the pass"
                Expect.equal run.Infos.Length categories.Length "the run is unchanged")
        testCase "a broken syllable of repha glyphs agrees" (fun () ->
            let categories = [| IndicMachine.Repha; IndicMachine.Repha; IndicMachine.Repha |]
            let expected = oracle fontHandle IndicMachine.machine indicParams BufferFlags.Default categories
            let actual = ours font IndicMachine.machine indicParams BufferFlags.Default categories
            Expect.equal actual expected "all-repha broken syllable")
    ]
