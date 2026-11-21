module Farfbuzz.Tests.Layout.MatchingTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>An empty GDEF for the matching tests.</summary>
let private emptyGdef : Gdef =
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
let private infoOf (codepoint: uint32) (mask: uint32) : GlyphInfo =
    { Codepoint = codepoint; Mask = mask; Cluster = 0u; Var1 = 0u; Var2 = 0u }

/// <summary>Builds glyph properties from a code point and a glyph class.</summary>
/// <param name="codepoint">The code point.</param>
/// <param name="glyphClass">The glyph class.</param>
/// <returns>The properties.</returns>
let private propsOf (codepoint: uint32) (glyphClass: uint16) : GlyphProps =
    let struct (unicodeProps, _) = UnicodeProps.compute codepoint
    { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicodeProps; GlyphClass = glyphClass }

/// <summary>Builds a matcher with a full mask.</summary>
/// <param name="lookupProps">The lookup props.</param>
/// <param name="ignoreZwnj">Whether ZWNJ is ignored.</param>
/// <param name="ignoreZwj">Whether ZWJ is ignored.</param>
/// <param name="ignoreHidden">Whether hidden glyphs are ignored.</param>
/// <param name="perSyllable">Whether matching is per syllable.</param>
/// <param name="syllable">The syllable.</param>
/// <returns>The matcher.</returns>
let private matcherOf (lookupProps: uint32) (ignoreZwnj: bool) (ignoreZwj: bool) (ignoreHidden: bool) (perSyllable: bool) (syllable: byte) : Matcher =
    { LookupProps = lookupProps
      Mask = 0xFFFFFFFFu
      IgnoreZwnj = ignoreZwnj
      IgnoreZwj = ignoreZwj
      IgnoreHidden = ignoreHidden
      PerSyllable = perSyllable
      Syllable = syllable }

/// <summary>The list model of skipping_iterator_t::next.</summary>
/// <param name="infos">The glyph infos.</param>
/// <param name="props">The glyph properties.</param>
/// <param name="m">The matcher.</param>
/// <param name="gdef">The GDEF.</param>
/// <param name="data">The glyph data.</param>
/// <param name="position">The glyph-data index.</param>
/// <param name="index">The cursor index.</param>
/// <param name="finish">The exclusive end index.</param>
/// <returns>The hit, or the unsafe bound of a miss.</returns>
let rec private modelNext (infos: GlyphInfo list) (props: GlyphProps list) (m: Matcher) (gdef: Gdef) (data: MatchAlways) (position: int) (index: int) (finish: int) : Probe =
    match index + 1 < finish with
    | false -> Miss finish
    | true ->
        let j = index + 1
        match Matching.verdict m gdef data position (List.item j infos) (List.item j props) with
        | Matched -> Hit j
        | NotMatched -> Miss (j + 1)
        | Skipped -> modelNext infos props m gdef data position j finish

/// <summary>The list model of skipping_iterator_t::prev.</summary>
/// <param name="infos">The glyph infos.</param>
/// <param name="props">The glyph properties.</param>
/// <param name="m">The matcher.</param>
/// <param name="gdef">The GDEF.</param>
/// <param name="data">The glyph data.</param>
/// <param name="position">The glyph-data index.</param>
/// <param name="index">The cursor index.</param>
/// <returns>The hit, or the unsafe bound of a miss.</returns>
let rec private modelPrev (infos: GlyphInfo list) (props: GlyphProps list) (m: Matcher) (gdef: Gdef) (data: NoMatchFunc) (position: int) (index: int) : Probe =
    match index > 0 with
    | false -> Miss 0
    | true ->
        let j = index - 1
        match Matching.verdict m gdef data position (List.item j infos) (List.item j props) with
        | Matched -> Hit j
        | NotMatched -> Miss (max 1 j - 1)
        | Skipped -> modelPrev infos props m gdef data position j

/// <summary>Generates a glyph run and a matcher over ignorables, marks and ignore flags.</summary>
/// <returns>The generator.</returns>
let private genRunAndMatcher : Gen<GlyphInfo list * GlyphProps list * Matcher> =
    gen {
        let! length = Gen.choose (0, 12)
        let! codepoints = Gen.listOfLength length (Gen.elements [| 0x41u; 0x42u; 0x200Cu; 0x200Du; 0x034Fu; 0x2060u |])
        let! masks = Gen.listOfLength length (Gen.elements [| 0u; 1u; 0xFFFFFFFFu |])
        let! classes = Gen.listOfLength length (Gen.elements [| 0us; GlyphPropsBits.Base; GlyphPropsBits.Ligature; GlyphPropsBits.Mark |])
        let infos = List.map2 (fun c mask -> infoOf c mask) codepoints masks
        let props = List.map2 propsOf codepoints classes
        let! lookupProps = Gen.elements [| 0u; LookupFlag.IgnoreBaseGlyphs; LookupFlag.IgnoreLigatures; LookupFlag.IgnoreMarks; LookupFlag.IgnoreFlags |]
        let! ignoreZwnj = Gen.elements [| false; true |]
        let! ignoreZwj = Gen.elements [| false; true |]
        let! ignoreHidden = Gen.elements [| false; true |]
        let m = matcherOf lookupProps ignoreZwnj ignoreZwj ignoreHidden false 0uy
        return infos, props, m
    }

/// <summary>Generates a run, matcher, cursor index and finish for the next model.</summary>
/// <returns>The generator.</returns>
let private genNextCase : Gen<GlyphInfo list * GlyphProps list * Matcher * int * int> =
    gen {
        let! (infos, props, m) = genRunAndMatcher
        let length = List.length infos
        let! index = Gen.choose (0, max 0 (length - 1))
        let! finish = Gen.choose (index, length)
        return infos, props, m, index, finish
    }

/// <summary>Generates a run, matcher and cursor index for the prev model.</summary>
/// <returns>The generator.</returns>
let private genPrevCase : Gen<GlyphInfo list * GlyphProps list * Matcher * int> =
    gen {
        let! (infos, props, m) = genRunAndMatcher
        let length = List.length infos
        let! index = Gen.choose (0, length)
        return infos, props, m, index
    }

/// <summary>An edit of the run-pass model, the new in-place cases and the index-reading companion.</summary>
[<Struct>]
type private NewEdit =
    | InputProps of inputIndex: int * inputProps: GlyphProps
    | Position of positionIndex: int * position: GlyphPosition
    | Index of index: int
    | Serial of serial: byte
    | RandomState of randomState: uint32
    | Scratch of scratch: ScratchFlags
    | CurrentProps of currentProps: GlyphProps

/// <summary>A pass that replays the new-edit script.</summary>
[<Struct>]
type private NewEditPass =
    val Edits: NewEdit array
    /// <summary>Creates a replay pass.</summary>
    /// <param name="edits">The edit script.</param>
    new (edits) = { Edits = edits }
    interface IRunPass<int> with
        member this.Step (_, index) =
            match index >= this.Edits.Length with
            | true -> struct (RunEdit.Finish, index)
            | false ->
                match this.Edits[index] with
                | InputProps (i, p) -> struct (RunEdit.SetInputProps (i, p), index + 1)
                | Position (i, p) -> struct (RunEdit.SetPosition (i, p), index + 1)
                | Index i -> struct (RunEdit.SetIndex i, index + 1)
                | Serial s -> struct (RunEdit.SetSerial s, index + 1)
                | RandomState s -> struct (RunEdit.SetRandomState s, index + 1)
                | Scratch s -> struct (RunEdit.AddScratch s, index + 1)
                | CurrentProps p -> struct (RunEdit.SetCurrentProps p, index + 1)

/// <summary>The list model of the in-place machine fields the new edits touch.</summary>
type private NewEditModel =
    { Props: GlyphProps array
      Positions: GlyphPosition array
      Idx: int
      Serial: uint32
      RandomState: uint32
      Scratch: ScratchFlags }

/// <summary>Replaces one element of an array.</summary>
/// <param name="items">The items.</param>
/// <param name="index">The index.</param>
/// <param name="value">The value.</param>
/// <returns>The array with the value.</returns>
let private setArr (items: 'T array) (index: int) (value: 'T) : 'T array =
    let result = Array.copy items
    result[index] <- value
    result

/// <summary>Applies one edit to the model.</summary>
/// <param name="model">The model.</param>
/// <param name="edit">The edit.</param>
/// <returns>The next model.</returns>
let rec private modelApply (model: NewEditModel) (edit: NewEdit) : NewEditModel =
    match edit with
    | InputProps (index, props) -> { model with Props = setArr model.Props index props }
    | Position (index, position) -> { model with Positions = setArr model.Positions index position }
    | Index index -> { model with Idx = index }
    | Serial serial -> { model with Serial = uint32 serial }
    | RandomState state -> { model with RandomState = state }
    | Scratch scratch -> { model with Scratch = model.Scratch ||| scratch }
    | CurrentProps props -> { model with Props = setArr model.Props model.Idx props }

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

/// <summary>Generates a random glyph position.</summary>
/// <returns>The generator.</returns>
let private genPosition : Gen<GlyphPosition> =
    gen {
        let! xAdvance = Gen.choose (-1000, 1000)
        let! yAdvance = Gen.choose (-1000, 1000)
        let! xOffset = Gen.choose (-1000, 1000)
        let! yOffset = Gen.choose (-1000, 1000)
        let! var = Gen.choose (0, 1000) |> Gen.map uint32
        return { XAdvance = xAdvance; YAdvance = yAdvance; XOffset = xOffset; YOffset = yOffset; Var = var }
    }

/// <summary>Generates one new edit with a valid index.</summary>
/// <param name="length">The run length.</param>
/// <returns>The edit generator.</returns>
let private genNewEdit (length: int) : Gen<NewEdit> =
    Gen.oneof [
        gen {
            let! i = Gen.choose (0, length - 1)
            let! p = genProps
            return InputProps (i, p)
        }
        gen {
            let! i = Gen.choose (0, length - 1)
            let! p = genPosition
            return Position (i, p)
        }
        gen {
            let! i = Gen.choose (0, length - 1)
            return Index i
        }
        gen {
            let! s = Gen.choose (0, 255) |> Gen.map byte
            return Serial s
        }
        gen {
            let! s = Gen.choose (0, Int32.MaxValue) |> Gen.map uint32
            return RandomState s
        }
        gen {
            let! s = Gen.elements [| ScratchFlags.Default; ScratchFlags.HasCgj; ScratchFlags.HasDefaultIgnorables; ScratchFlags.HasContinuations |]
            return Scratch s
        }
        gen {
            let! p = genProps
            return CurrentProps p
        }
    ]

/// <summary>Generates the run and edit script for the new-edit model.</summary>
/// <returns>The generator.</returns>
let private genNewEditInput : Gen<GlyphInfo array * GlyphProps array * GlyphPosition array * uint32 * uint32 * NewEdit array> =
    gen {
        let! length = Gen.choose (1, 6)
        let! codepoints = Gen.listOfLength length (Gen.choose (0, 200) |> Gen.map uint32)
        let! props = Gen.listOfLength length genProps
        let! positions = Gen.listOfLength length genPosition
        let infos = codepoints |> List.mapi (fun i c -> { Codepoint = c; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u }) |> Array.ofList
        let! serial = Gen.choose (0, 255) |> Gen.map uint32
        let! randomState = Gen.choose (0, Int32.MaxValue) |> Gen.map uint32
        let! editCount = Gen.choose (1, 10)
        let! edits = Gen.listOfLength editCount (genNewEdit length)
        return infos, Array.ofList props, Array.ofList positions, serial, randomState, Array.ofList edits
    }

/// <summary>The P5.S05 matching and run-pass cases.</summary>
[<Tests>]
let tests =
    testList "P5.S05" [
        test "maySkip classifies ignorables, ZWJ, ZWNJ and hidden glyphs" {
            let gdef = emptyGdef
            Expect.equal (Matching.maySkip (matcherOf 0u false false false false 0uy) gdef (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u 0us)) SkipNo "a letter is not skipped"
            Expect.equal (Matching.maySkip (matcherOf 0u false false false false 0uy) gdef (infoOf 0x2060u 0xFFFFFFFFu) (propsOf 0x2060u 0us)) SkipMaybe "a plain ignorable is maybe-skipped"
            Expect.equal (Matching.maySkip (matcherOf 0u false false false false 0uy) gdef (infoOf 0x200Cu 0xFFFFFFFFu) (propsOf 0x200Cu 0us)) SkipNo "ZWNJ kept without the flag"
            Expect.equal (Matching.maySkip (matcherOf 0u true false false false 0uy) gdef (infoOf 0x200Cu 0xFFFFFFFFu) (propsOf 0x200Cu 0us)) SkipMaybe "ZWNJ skipped with the flag"
            Expect.equal (Matching.maySkip (matcherOf 0u false false false false 0uy) gdef (infoOf 0x200Du 0xFFFFFFFFu) (propsOf 0x200Du 0us)) SkipNo "ZWJ kept without the flag"
            Expect.equal (Matching.maySkip (matcherOf 0u false true false false 0uy) gdef (infoOf 0x200Du 0xFFFFFFFFu) (propsOf 0x200Du 0us)) SkipMaybe "ZWJ skipped with the flag"
            Expect.equal (Matching.maySkip (matcherOf 0u false false false false 0uy) gdef (infoOf 0x034Fu 0xFFFFFFFFu) (propsOf 0x034Fu 0us)) SkipNo "hidden glyph kept without the flag"
            Expect.equal (Matching.maySkip (matcherOf 0u false false true false 0uy) gdef (infoOf 0x034Fu 0xFFFFFFFFu) (propsOf 0x034Fu 0us)) SkipMaybe "hidden glyph skipped with the flag"
            Expect.equal (Matching.maySkip (matcherOf LookupFlag.IgnoreBaseGlyphs false false false false 0uy) gdef (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u GlyphPropsBits.Base)) SkipYes "an ignored base is skipped"
        }
        test "verdict covers the skip, mask and match table" {
            let gdef = emptyGdef
            let m = matcherOf 0u false false false false 0uy
            let noMask = { m with Mask = 0u }
            Expect.equal (Matching.verdict (matcherOf LookupFlag.IgnoreBaseGlyphs false false false false 0uy) gdef (MatchAlways()) 0 (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u GlyphPropsBits.Base)) Skipped "SkipYes gives Skipped"
            Expect.equal (Matching.verdict m gdef (MatchAlways()) 0 (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u 0us)) Matched "a matching glyph gives Matched"
            Expect.equal (Matching.verdict m gdef (NoMatchFunc()) 0 (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u 0us)) Matched "no match func with SkipNo gives Matched"
            Expect.equal (Matching.verdict noMask gdef (NoMatchFunc()) 0 (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u 0us)) NotMatched "a masked-out glyph gives NotMatched"
            Expect.equal (Matching.verdict m gdef (MatchAlways()) 0 (infoOf 0x2060u 0xFFFFFFFFu) (propsOf 0x2060u 0us)) Matched "SkipMaybe with a match gives Matched"
            Expect.equal (Matching.verdict m gdef (NoMatchFunc()) 0 (infoOf 0x2060u 0xFFFFFFFFu) (propsOf 0x2060u 0us)) Skipped "SkipMaybe without a match func gives Skipped"
            Expect.equal (Matching.verdict noMask gdef (MatchAlways()) 0 (infoOf 0x2060u 0xFFFFFFFFu) (propsOf 0x2060u 0us)) Skipped "SkipMaybe with a masked-out glyph gives Skipped"
            let ids = GlyphIds (Sealed.ofSpan (ReadOnlySpan [| 99u |]))
            Expect.equal (Matching.verdict m gdef ids 0 (infoOf 0x41u 0xFFFFFFFFu) (propsOf 0x41u 0us)) NotMatched "a missed match func gives NotMatched"
        }
        testProperty "next matches a list model" (
            Prop.forAll (Arb.fromGen genNextCase) (fun (infos, props, m, index, finish) ->
                let gdef = emptyGdef
                let infosArray = Array.ofList infos
                let propsArray = Array.ofList props
                let ours = Matching.next m gdef (MatchAlways()) 0 (ReadOnlySpan infosArray) (ReadOnlySpan propsArray) index finish |> TailCalls.settle
                let expected = modelNext infos props m gdef (MatchAlways()) 0 index finish
                ours = expected))
        testProperty "prev matches a list model" (
            Prop.forAll (Arb.fromGen genPrevCase) (fun (infos, props, m, index) ->
                let gdef = emptyGdef
                let infosArray = Array.ofList infos
                let propsArray = Array.ofList props
                let ours = Matching.prev m gdef (NoMatchFunc()) 0 (ReadOnlySpan infosArray) (ReadOnlySpan propsArray) index |> TailCalls.settle
                let expected = modelPrev infos props m gdef (NoMatchFunc()) 0 index
                ours = expected))
        test "a miss reports its unsafe bound" {
            let gdef = emptyGdef
            let m = matcherOf 0u false false false false 0uy
            let infos = [| infoOf 0x41u 0xFFFFFFFFu; infoOf 0x42u 0u; infoOf 0x43u 0xFFFFFFFFu |]
            let props = [| propsOf 0x41u 0us; propsOf 0x42u 0us; propsOf 0x43u 0us |]
            Expect.equal (Matching.next m gdef (MatchAlways()) 0 (ReadOnlySpan infos) (ReadOnlySpan props) 0 3 |> TailCalls.settle) (Miss 2) "next reports idx+1 on a miss"
            Expect.equal (Matching.next m gdef (MatchAlways()) 0 (ReadOnlySpan infos) (ReadOnlySpan props) 2 3 |> TailCalls.settle) (Miss 3) "next at the end reports finish"
            Expect.equal (Matching.prev m gdef (MatchAlways()) 0 (ReadOnlySpan infos) (ReadOnlySpan props) 2 |> TailCalls.settle) (Miss 0) "prev reports max(1,idx)-1 on a miss"
            Expect.equal (Matching.prev m gdef (MatchAlways()) 0 (ReadOnlySpan infos) (ReadOnlySpan props) 0 |> TailCalls.settle) (Miss 0) "prev at the start reports 0"
        }
        test "checkGlyphProperty filters mark attachment types and mark filtering sets" {
            Expect.isTrue (Matching.checkGlyphProperty emptyGdef 10u (GlyphPropsBits.Mark ||| uint16 (3 <<< 8)) (uint32 (3 <<< 8))) "a matching attachment class passes"
            Expect.isFalse (Matching.checkGlyphProperty emptyGdef 10u (GlyphPropsBits.Mark ||| uint16 (3 <<< 8)) (uint32 (4 <<< 8))) "a different attachment class fails"
            let coverage = Coverage.parse (ReadOnlySpan (Array.concat [ be16 1u; be16 2u; be16 10u; be16 20u ])) 0
            let gdef = { emptyGdef with MarkGlyphSets = Sealed.ofSpan (ReadOnlySpan [| coverage |]) }
            Expect.isTrue (Matching.checkGlyphProperty gdef 10u GlyphPropsBits.Mark LookupFlag.UseMarkFilteringSet) "a mark in the set passes"
            Expect.isFalse (Matching.checkGlyphProperty gdef 30u GlyphPropsBits.Mark LookupFlag.UseMarkFilteringSet) "a mark outside the set fails"
        }
        test "allocateLigId wraps after 255 serials and retries a zero lig id" {
            Expect.equal (Matching.nextSerial 0uy) 1uy "nextSerial 0 is 1"
            Expect.equal (Matching.nextSerial 7uy) 8uy "nextSerial 7 is 8"
            Expect.equal (Matching.nextSerial 255uy) 1uy "nextSerial 255 wraps to 1"
            let struct (ligId, serial) = Matching.allocateLigId 255uy
            Expect.equal ligId 1uy "the lig id after the wrap is 1"
            Expect.equal serial 1uy "the serial after the wrap is 1"
            let struct (retried, advanced) = Matching.allocateLigId 7uy
            Expect.equal retried 1uy "the lig id retries past the zero of 8"
            Expect.equal advanced 9uy "the serial advanced past the zero lig id"
        }
        testProperty "the new run edits match a list model" (
            Prop.forAll (Arb.fromGen genNewEditInput) (fun (infos, props, positions, serial, randomState, edits) ->
                let run =
                    { GlyphRun.empty with
                        Infos = Sealed.ofSpan (ReadOnlySpan infos)
                        Props = Sealed.ofSpan (ReadOnlySpan props)
                        Positions = Sealed.ofSpan (ReadOnlySpan positions)
                        HavePositions = true
                        Serial = serial
                        RandomState = randomState }
                let shaped = GlyphRun.runPass (NewEditPass(edits)) 0 run
                let model =
                    List.fold modelApply
                        { Props = props; Positions = positions; Idx = 0; Serial = serial; RandomState = randomState; Scratch = ScratchFlags.Default }
                        (List.ofArray edits)
                shaped.Props.AsSpan().ToArray() = model.Props
                && shaped.Positions.AsSpan().ToArray() = model.Positions
                && shaped.Serial = model.Serial
                && shaped.RandomState = model.RandomState
                && shaped.ScratchFlags = model.Scratch
                && shaped.Props.Length = shaped.Infos.Length))
    ]
