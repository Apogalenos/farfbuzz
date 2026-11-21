module Farfbuzz.Tests.Shape.LineShapeTests

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles
open Farfbuzz.Tests.Shape.ShapeManyTests

/// <summary>The layout-table tags a stripped copy of a font renames away.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>A copy of a font with its layout tables renamed, so that its lines are lookup free.</summary>
/// <param name="original">The font bytes.</param>
/// <returns>The stripped font bytes.</returns>
let stripped (original: ReadOnlyMemory<byte>) =
    let bytes = original.ToArray()
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    for index in 0 .. count - 1 do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
    ReadOnlyMemory<byte>(bytes)

/// <summary>The faces the properties shape with: Roboto with and without layout, Amiri, Devanagari, a variable Latin font, and four
/// complex-script faces that map few or no Latin letters: Myanmar, Harmattan (Arabic), AdobeBlank (no glyph for any letter) and Khmer.</summary>
let private fonts =
    lazy
        (let roboto = read "perf/fonts/Roboto-Regular.ttf"
         [| Font.create (Face.create roboto 0u)
            Font.create (Face.create (stripped roboto) 0u)
            Font.create (Face.create (read "perf/fonts/Amiri-Regular.ttf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/SourceSerifVariable-Roman.ttf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/NotoSerifMyanmar-Regular.otf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/Harmattan-Regular.ttf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/AdobeBlank-Regular.ttf") 0u)
            Font.create (Face.create (read "test/subset/data/fonts/Khmer.ttf") 0u) |])

/// <summary>Code point ranges of the scripts the documents mix: Latin, Latin extended, Greek, Cyrillic, Hebrew with points, Thai, Lao,
/// Hangul, Khmer, Devanagari with its signs, Arabic, Myanmar, combining marks, joiners, neutral digits and punctuation and the
/// supplementary planes.</summary>
let private ranges =
    [| struct (0x41, 0x7A); struct (0x41, 0x7A); struct (0x61, 0x7A); struct (0xA0, 0x24F); struct (0x391, 0x3C9); struct (0x410, 0x44F)
       struct (0x5D0, 0x5EA); struct (0x591, 0x5C7); struct (0xE01, 0xE3A); struct (0xE81, 0xEAE); struct (0xAC00, 0xAC40)
       struct (0x1780, 0x17B3); struct (0x905, 0x939); struct (0x93C, 0x94D); struct (0x627, 0x64A); struct (0x1000, 0x1021)
       struct (0x300, 0x36F); struct (0x200C, 0x200D); struct (0x20, 0x40); struct (0x20, 0x40); struct (0x1F600, 0x1F64F); struct (0x10400, 0x1044F) |]

/// <summary>The ranges a lean-lane document draws from: mostly the plain Latin letters and spaces the kernel takes.</summary>
let private latinRanges = [| struct (0x61, 0x7A); struct (0x61, 0x7A); struct (0x66, 0x6C); struct (0x66, 0x6C); struct (0x41, 0x5A); struct (0x20, 0x20); struct (0x20, 0x40); struct (0xE0, 0xFF); struct (0x300, 0x303) |]

/// <summary>The pieces of a ligature-heavy Latin line: the letters that form ligatures (<c>fi</c>, <c>fl</c>, <c>ff</c>, <c>ffi</c>,
/// <c>ffl</c>, <c>fj</c>, <c>ft</c>), pairs the font kerns, spaces, digits, accented letters and a precomposed ligature.</summary>
let private ligaturePieces =
    [| "f"; "f"; "f"; "fi"; "fi"; "fl"; "ffi"; "ffl"; "ff"; "fj"; "ft"; "i"; "l"; "t"; "j"; "e"; "o"; "a"; "AV"; "To"; "Wa"; "Ty"; "P."; "r,"
       " "; " "; " "; " "; "1"; "\u00E9"; "\u00FC"; "\uFB01" |]

/// <summary>The pieces that send a ligature-heavy line to another route: a combining mark, a joiner and the fraction slash.</summary>
let private ligatureRarePieces = [| "\u0323"; "\u200D"; "\u2044" |]

/// <summary>One random ligature-heavy line: its code points.</summary>
let private ligatureLineGen =
    gen {
        let! pieces = Gen.frequency [ (1, Gen.constant 0); (1, Gen.constant 1); (4, Gen.choose (2, 8)); (6, Gen.choose (9, 50)) ]
        let! chosen = Gen.arrayOfLength pieces (Gen.frequency [ (200, Gen.elements ligaturePieces); (1, Gen.elements ligatureRarePieces) ])
        return chosen |> Array.collect (fun piece -> piece.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Array.ofSeq)
    }

/// <summary>The pieces of a line of Latin text, digits and punctuation that a complex-script face maps in part or not at all: words,
/// spaces, digits, quotation marks, dashes, an accented letter whose decomposition the face may lack, a no-break space, a no-break
/// hyphen, a tab, a soft hyphen, letter-like symbols, three of which (Kelvin, Ohm and Angstrom) decompose to a single letter, the
/// spaces of other widths, a zero-width space and a line separator.</summary>
let private latinPieces =
    [| "The"; "little"; "prince"; "and"; "Hello"; "world"; "of"; "fox"; "rose"; "ZZ"; " "; " "
       " "; " "; " "; "0"; "12"; "2026"; "."; ","; "-"; "("; ")"; "\u2019"
       "\u201C"; "\u201D"; "\u2014"; "\u2013"; "\u00E9"; "\u00FC"; "\u00C0"; "\u00A0"; "\u2011"; "\t"; "\u00AD"; "\u2122"
       "\u00DF"; "\u0131"; "\u212A"; "\u2126"; "\u212B"; "\u2000"; "\u2009"; "\u3000"; "\u200B"; "\u2028" |]

/// <summary>The pieces of complex-script words that a mixed line carries now and then: Devanagari, Arabic, Myanmar and Khmer words, a
/// combining mark, a joiner, a variation selector and a Hangul syllable.</summary>
let private complexPieces =
    [| "\u0928\u092E\u0938\u094D\u0924\u0947"; "\u0939\u093F\u0928\u094D\u0926\u0940"; "\u0645\u0631\u062D\u0628\u0627"; "\u0627\u0644\u0639\u0631\u0628\u064A\u0629"; "\u1019\u103C\u1014\u103A\u1019\u102C"; "\u1780\u17D2\u1798\u17C2\u179A"; "\u0301"; "\u200D"; "\uFE0F"; "\uAC00" |]

/// <summary>One random line of Latin text, digits and punctuation, with now and then a word of a complex script: its code points.</summary>
let private mixedLineGen =
    gen {
        let! pieces = Gen.frequency [ (1, Gen.constant 0); (1, Gen.constant 1); (4, Gen.choose (2, 8)); (6, Gen.choose (9, 50)) ]
        let! complexShare = Gen.elements [| 0; 0; 0; 1; 5; 30 |]
        let! chosen =
            Gen.arrayOfLength pieces (Gen.frequency [ (100 - complexShare, Gen.elements latinPieces); (complexShare, Gen.elements complexPieces) ])
        return chosen |> Array.collect (fun piece -> piece.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Array.ofSeq)
    }

/// <summary>The text of a document in one encoding.</summary>
type private Body =
    | Utf8Body of byte array
    | Utf16Body of uint16 array
    | Utf32Body of uint32 array
    | CodepointBody of uint32 array

/// <summary>One random document and everything that shapes it.</summary>
type private Case =
    { FontIndex: int
      Features: ImmutableArray<Feature>
      Shapers: ImmutableArray<string> voption
      Settings: GlyphRun
      Language: Language
      Body: Body
      Lines: struct (int * int) array
      Degree: int
      Threshold: int
      Warm: bool }

/// <summary>The feature lists the documents use.</summary>
let private featureSets =
    [| ImmutableArray<Feature>.Empty
       ImmutableArray<Feature>.Empty
       ImmutableArray.Create { Tag = Tag.ofString "kern"; Value = 0u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
       ImmutableArray.Create { Tag = Tag.ofString "liga"; Value = 0u; Start = 2u; End = 7u }
       ImmutableArray.Create { Tag = Tag.ofString "smcp"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd } |]

/// <summary>The shaper lists the documents use: the default, the ot shaper, the fallback shaper and one that names no shaper.</summary>
let private shaperSets =
    [| ValueNone; ValueSome (ImmutableArray.Create "ot"); ValueSome (ImmutableArray.Create "fallback"); ValueSome (ImmutableArray.Create "nothing") |]

/// <summary>The buffer flags the documents use.</summary>
let private flagSets =
    [| BufferFlags.Default; BufferFlags.Default; BufferFlags.Default; BufferFlags.Default; BufferFlags.Verify; BufferFlags.Bot ||| BufferFlags.Eot
       BufferFlags.PreserveDefaultIgnorables; BufferFlags.RemoveDefaultIgnorables; BufferFlags.ProduceUnsafeToConcat
       BufferFlags.ProduceSafeToInsertTatweel; BufferFlags.DoNotInsertDottedCircle; BufferFlags.Verify ||| BufferFlags.ProduceUnsafeToConcat |]

/// <summary>The settings every line of a document has: flags, cluster level, replacement, forced segment properties and limits.</summary>
let private settingsGen : Gen<GlyphRun> =
    gen {
        let! flags = Gen.elements flagSets
        let! level = Gen.elements [| ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes |]
        let! replacement = Gen.elements [| 0xFFFDu; 0xFFFDu; 0x3Fu; 0u; 0x41u |]
        let! direction = Gen.elements [| Direction.Invalid; Direction.Invalid; Direction.Invalid; Direction.Invalid; Direction.Ltr; Direction.Rtl; Direction.Ttb |]
        let! script = Gen.elements [| Scripts.Invalid; Scripts.Invalid; Scripts.Invalid; Scripts.Latin; Scripts.Arabic |]
        let! maxOps = Gen.elements [| GlyphRun.MaxOpsDefault; GlyphRun.MaxOpsDefault; GlyphRun.MaxOpsDefault; GlyphRun.MaxOpsDefault; 3; 40; 300 |]
        let! maxLen = Gen.elements [| GlyphRun.MaxLenDefault; GlyphRun.MaxLenDefault; GlyphRun.MaxLenDefault; GlyphRun.MaxLenDefault; 2; 10 |]
        let! notFound = Gen.elements [| 0u; 0u; 0u; 0u; 0u; 3u; 77u; 0xFFFFu |]
        return
            { GlyphRun.empty with
                NotFound = notFound
                Flags = flags
                ClusterLevel = level
                Replacement = replacement
                SegmentProperties = { Direction = direction; Script = script; Language = Language.NoLanguage }
                MaxOps = maxOps
                MaxLen = maxLen }
    }

/// <summary>One random line: its code points.</summary>
/// <param name="pool">The ranges the code points come from.</param>
/// <returns>The generator.</returns>
let private lineGen (pool: struct (int * int) array) : Gen<uint32 array> =
    gen {
        let! length = Gen.frequency [ (1, Gen.constant 0); (1, Gen.constant 1); (4, Gen.choose (2, 8)); (6, Gen.choose (9, 60)) ]
        let! mixed = Gen.choose (0, 4)
        let! primary = Gen.elements pool
        let pick =
            gen {
                let! range = match mixed with | 0 -> Gen.elements pool | _ -> Gen.constant primary
                let struct (low, high) = range
                let! code = Gen.choose (low, high)
                return uint32 code
            }
        return! Gen.arrayOfLength length pick
    }

/// <summary>Appends a code point to a list of UTF-8 bytes, as a scalar value or as a lone byte for what is none.</summary>
/// <param name="bytes">The bytes so far.</param>
/// <param name="codepoint">The code point.</param>
let private appendUtf8 (bytes: List<byte>) (codepoint: uint32) =
    match codepoint with
    | 0xFFFFFFFFu -> bytes.Add 0xFFuy
    | 0xFFFFFFFEu -> bytes.Add 0x80uy
    | 0xFFFFFFFDu ->
        bytes.Add 0xE2uy
        bytes.Add 0x82uy
    | value when value >= 0xD800u && value <= 0xDFFFu || value > 0x10FFFFu -> bytes.Add 0xC0uy
    | value -> bytes.AddRange (System.Text.Encoding.UTF8.GetBytes (System.Text.Rune(int value).ToString()))

/// <summary>Appends a code point to a list of UTF-16 units, a lone surrogate included.</summary>
/// <param name="units">The units so far.</param>
/// <param name="codepoint">The code point.</param>
let private appendUtf16 (units: List<uint16>) (codepoint: uint32) =
    match codepoint with
    | value when value >= 0x10000u && value <= 0x10FFFFu ->
        let offset = value - 0x10000u
        units.Add (uint16 (0xD800u + (offset >>> 10)))
        units.Add (uint16 (0xDC00u + (offset &&& 0x3FFu)))
    | value -> units.Add (uint16 (value &&& 0xFFFFu))

/// <summary>The encoded text of a document, its lines each followed by a newline, and where each line is, with some lines given
/// offsets and lengths that stray outside the text.</summary>
/// <param name="encoding">Which encoding, from 0 to 3.</param>
/// <param name="lines">The code points of each line.</param>
/// <param name="strays">Which lines stray.</param>
/// <returns>The text and the offset and length of each line.</returns>
let private encoded (encoding: int) (lines: uint32 array array) (strays: int array) =
    let units = List<uint32>()
    let bytes = List<byte>()
    let shorts = List<uint16>()
    let starts = Array.zeroCreate<int> lines.Length
    let lengths = Array.zeroCreate<int> lines.Length
    for index in 0 .. lines.Length - 1 do
        match encoding with
        | 0 ->
            starts[index] <- bytes.Count
            for codepoint in lines[index] do appendUtf8 bytes codepoint
            lengths[index] <- bytes.Count - starts[index]
            bytes.Add 10uy
        | 1 ->
            starts[index] <- shorts.Count
            for codepoint in lines[index] do appendUtf16 shorts codepoint
            lengths[index] <- shorts.Count - starts[index]
            shorts.Add 10us
        | _ ->
            starts[index] <- units.Count
            units.AddRange lines[index]
            lengths[index] <- units.Count - starts[index]
            units.Add 10u
    let total = match encoding with | 0 -> bytes.Count | 1 -> shorts.Count | _ -> units.Count
    let spec (index: int) =
        match strays[index] with
        | 1 -> struct (-1, 3)
        | 2 -> struct (total + 5, 2)
        | 3 -> struct (starts[index], -1)
        | 4 -> struct (starts[index], lengths[index] + 1000)
        | 5 -> struct (max 0 (starts[index] - 2), -4)
        | _ -> struct (starts[index], lengths[index])
    let text =
        match encoding with
        | 0 -> Utf8Body (bytes.ToArray())
        | 1 -> Utf16Body (shorts.ToArray())
        | 2 -> Utf32Body (units.ToArray())
        | _ -> CodepointBody (units.ToArray())
    struct (text, Array.init lines.Length spec)

/// <summary>Code points that are no scalar values, for the encodings that carry them.</summary>
let private oddCodepoints = [| 0xD800u; 0xDFFFu; 0x110000u; 0xFFFFFFFFu; 0xFFFFFFFEu; 0xFFFFFFFDu; 0x0u |]

/// <summary>A random document: no lines, one line, a few, many, or enough glyphs to be shaped on several threads.</summary>
/// <param name="lineGenerator">The generator of one line's code points.</param>
/// <param name="fontChoices">The fonts it may use.</param>
/// <returns>The generator.</returns>
let private caseGenOf (lineGenerator: Gen<uint32 array>) (fontChoices: int array) : Gen<Case> =
    gen {
        let! fontIndex = Gen.elements fontChoices
        let! features = Gen.elements featureSets
        let! shapers = Gen.frequency [ (12, Gen.constant shaperSets[0]); (3, Gen.constant shaperSets[1]); (2, Gen.constant shaperSets[2]); (1, Gen.constant shaperSets[3]) ]
        let! settings = settingsGen
        let! language = Gen.elements [| Language.cLocale; Language.ofString "en"; Language.NoLanguage |]
        let! encoding = Gen.choose (0, 3)
        let! degree = Gen.elements [| 1; 2; 4; Shape.AllCores |]
        let! threshold = Gen.elements [| 0; 0; Shape.SequentialBelowGlyphs |]
        let! lineCount = Gen.frequency [ (3, Gen.choose (0, 1)); (6, Gen.choose (2, 5)); (6, Gen.choose (6, 60)); (2, Gen.choose (150, 300)) ]
        let! lines = Gen.arrayOfLength lineCount lineGenerator
        let! odd = Gen.frequency [ (9, Gen.constant false); (1, Gen.constant true) ]
        let! oddPicks = Gen.arrayOfLength lineCount (Gen.choose (0, 9))
        let! oddAt = Gen.arrayOfLength lineCount (Gen.choose (0, 12))
        let! oddValue = Gen.arrayOfLength lineCount (Gen.elements oddCodepoints)
        let! strays = Gen.arrayOfLength lineCount (Gen.frequency [ (60, Gen.constant 0); (1, Gen.choose (1, 5)) ])
        let! warm = Gen.elements [| false; true |]
        let damaged =
            lines
            |> Array.mapi (fun index line ->
                match odd && oddPicks[index] < 4 && line.Length > 0 with
                | true -> line |> Array.mapi (fun at codepoint -> match at = oddAt[index] % line.Length with | true -> oddValue[index] | false -> codepoint)
                | false -> line)
        let struct (text, specs) = encoded encoding damaged strays
        return { FontIndex = fontIndex; Features = features; Shapers = shapers; Settings = settings; Language = language; Body = text; Lines = specs; Degree = degree; Threshold = threshold; Warm = warm }
    }

/// <summary>A random document whose lines draw their code points from ranges.</summary>
/// <param name="pool">The ranges the code points come from.</param>
/// <param name="fontChoices">The fonts it may use.</param>
/// <returns>The generator.</returns>
let private caseGen (pool: struct (int * int) array) (fontChoices: int array) =
    caseGenOf (lineGen pool) fontChoices

/// <summary>The run a caller builds for a line: the text appended to the settings and the segment properties guessed.</summary>
/// <param name="case">The document.</param>
/// <param name="line">The offset and length of the line.</param>
/// <returns>The run.</returns>
let private runOf (case: Case) (line: struct (int * int)) =
    let struct (start, length) = line
    let run =
        match case.Body with
        | Utf8Body bytes -> GlyphRun.addUtf8 (ReadOnlySpan bytes) start length case.Settings |> TailCalls.settle
        | Utf16Body units -> GlyphRun.addUtf16 (ReadOnlySpan units) start length case.Settings |> TailCalls.settle
        | Utf32Body units -> GlyphRun.addUtf32 (ReadOnlySpan units) start length case.Settings |> TailCalls.settle
        | CodepointBody units -> GlyphRun.addCodepoints (ReadOnlySpan units) start length case.Settings |> TailCalls.settle
    GlyphRun.guessSegmentProperties case.Language run

/// <summary>Shapes a document with the entry point of its encoding.</summary>
/// <param name="case">The document.</param>
/// <param name="cache">The plan cache.</param>
/// <returns>The shaped document and the cache.</returns>
let private shapeLines (case: Case) (cache: PlanCache) =
    let font = fonts.Value[case.FontIndex]
    let lines = ImmutableArray.CreateRange case.Lines
    match case.Body with
    | Utf8Body bytes -> LineShape.utf8From case.Threshold case.Degree cache font case.Features case.Shapers case.Language case.Settings (ReadOnlyMemory bytes) lines
    | Utf16Body units -> LineShape.utf16From case.Threshold case.Degree cache font case.Features case.Shapers case.Language case.Settings (ReadOnlyMemory units) lines
    | Utf32Body units -> LineShape.utf32From case.Threshold case.Degree cache font case.Features case.Shapers case.Language case.Settings (ReadOnlyMemory units) lines
    | CodepointBody units -> LineShape.codepointsFrom case.Threshold case.Degree cache font case.Features case.Shapers case.Language case.Settings (ReadOnlyMemory units) lines

/// <summary>Whether a line of a shaped document equals the result of shaping the run built for it, glyph by glyph, position by
/// position, or error by error.</summary>
/// <param name="document">The shaped document.</param>
/// <param name="index">The line.</param>
/// <param name="expected">The result for the line.</param>
/// <returns>True when they agree.</returns>
let private sameLine (document: ShapedDocument) (index: int) (expected: Result<GlyphRun, ShapeError>) =
    match expected, ShapedDocument.errorOf document index with
    | Ok run, ValueNone ->
        run.Infos.AsEnumerable().SequenceEqual((ShapedDocument.infosOf document index).ToArray())
        && run.Positions.AsEnumerable().SequenceEqual((ShapedDocument.positionsOf document index).ToArray())
    | Error error, ValueSome refused -> sameResult (Error error) (Error refused)
    | Ok _, ValueSome _ -> false
    | Error _, ValueNone -> false

/// <summary>Whether the arrays of a document hold nothing outside its windows but zeros, and its windows lie inside them without
/// overlapping.</summary>
/// <param name="document">The shaped document.</param>
/// <returns>True when the arena is clean.</returns>
let private arenaIsClean (document: ShapedDocument) =
    let used = Array.zeroCreate<bool> document.ArenaInfos.Length
    let windows =
        document.Lines
        |> Seq.choose (fun outcome -> match outcome with | Arena (start, length) -> Some (struct (start, length)) | Alone _ -> None | Refused _ -> None)
        |> Array.ofSeq
    let inside = windows |> Array.forall (fun (struct (start, length)) -> start >= 0 && start + length <= used.Length)
    let disjoint =
        inside
        && windows
           |> Array.forall (fun (struct (start, length)) ->
               let free = Seq.forall (fun at -> not used[at]) (seq { start .. start + length - 1 })
               for at in start .. start + length - 1 do used[at] <- true
               free)
    let zeroInfo = Unchecked.defaultof<GlyphInfo>
    let zeroPosition = Unchecked.defaultof<GlyphPosition>
    disjoint
    && document.ArenaPositions.Length = document.ArenaInfos.Length
    && Seq.forall (fun at -> used[at] || (document.ArenaInfos[at] = zeroInfo && document.ArenaPositions[at] = zeroPosition)) (seq { 0 .. used.Length - 1 })

/// <summary>Whether two plan caches hold the same keys in the same order.</summary>
/// <param name="a">The first cache.</param>
/// <param name="b">The second cache.</param>
/// <returns>True when both caches hold equal keys in the same order.</returns>
let private sameCache (a: PlanCache) (b: PlanCache) =
    a.Entries.Length = b.Entries.Length
    && Seq.forall2 (fun (x: PlanEntry) (y: PlanEntry) -> EqualityComparer<ShapePlanKey>.Default.Equals(x.Key, y.Key)) a.Entries b.Entries

/// <summary>Whether the document entry equals <c>shapeCached</c> over the runs built for the lines, line by line and in the cache.</summary>
/// <param name="case">The document.</param>
/// <returns>True when both agree and the arena is clean.</returns>
let private equalsReference (case: Case) =
    let font = fonts.Value[case.FontIndex]
    let runs = case.Lines |> Array.map (runOf case)
    let start =
        match case.Warm, runs with
        | true, [||] -> PlanCache.empty
        | true, _ ->
            let struct (_, kept) = Shape.shapeCached PlanCache.empty font case.Features case.Shapers runs[0]
            kept
        | false, _ -> PlanCache.empty
    let struct (expected, expectedCache) = sequentialReference start font case.Features case.Shapers runs
    let struct (document, cache) = shapeLines case start
    document.Lines.Length = expected.Length
    && Seq.forall (fun index -> sameLine document index expected[index]) (seq { 0 .. expected.Length - 1 })
    && sameCache cache expectedCache
    && arenaIsClean document

/// <summary>Whether <c>Shape.shapeUtf8</c> equals decoding a line, guessing its properties and shaping it with <c>shapeCached</c>, in every
/// field of the run and in the cache, for every line of a UTF-8 document; a document of another encoding has nothing to compare.</summary>
/// <param name="case">The document.</param>
/// <returns>True when every line agrees.</returns>
let private utf8EntryEquals (case: Case) =
    match case.Body with
    | Utf16Body _ | Utf32Body _ | CodepointBody _ -> true
    | Utf8Body bytes ->
        let font = fonts.Value[case.FontIndex]
        case.Lines
        |> Array.forall (fun line ->
            let struct (start, length) = line
            let struct (expected, expectedCache) = Shape.shapeCached PlanCache.empty font case.Features case.Shapers (runOf case line)
            let struct (actual, cache) = Shape.shapeUtf8 PlanCache.empty font case.Features case.Shapers case.Language case.Settings (ReadOnlyMemory bytes) start length
            sameResult expected actual && sameCache cache expectedCache)

/// <summary>The run a facade hands the shaping entry: the line's settings, with no glyphs and no properties.</summary>
/// <param name="run">The decoded run of a line.</param>
/// <returns>The settings.</returns>
let private settingsOfRun (run: GlyphRun) =
    { run with Infos = ImmutableArray<GlyphInfo>.Empty; Props = ImmutableArray<GlyphProps>.Empty; Positions = ImmutableArray<GlyphPosition>.Empty }

/// <summary>Whether shaping a line into arrays of the caller equals <c>shapeCached</c> on its run (without the verify flag, which the entry does
/// not act on): into arrays that are long enough, into ones that are too short (which are left as they were and the length needed is
/// returned), and into the array the glyphs are in.</summary>
/// <param name="case">The document.</param>
/// <param name="index">The line.</param>
/// <returns>True when every way of shaping it agrees.</returns>
let private intoAgrees (case: Case) (index: int) =
    let font = fonts.Value[case.FontIndex]
    let run =
        let decoded = runOf case case.Lines[index]
        { decoded with Flags = decoded.Flags &&& ~~~BufferFlags.Verify }
    let struct (planned, _) = Shape.planFromCache PlanCache.empty font case.Features case.Shapers run
    match planned, run.Infos.Length with
    | Error _, _ -> true
    | Ok _, 0 -> true
    | Ok plan, count ->
        let struct (expected, _) = Shape.shapeCached PlanCache.empty font case.Features case.Shapers run
        let settings = settingsOfRun run
        let input = run.Infos.AsSpan().ToArray()
        let sentinelInfo = { Codepoint = 0xABCDu; Mask = 1u; Cluster = 2u; Var1 = 3u; Var2 = 4u }
        let sentinelPosition = { XAdvance = 9; YAdvance = 8; XOffset = 7; YOffset = 6; Var = 5u }
        let attempt (infoLength: int) (positionLength: int) =
            let infos = Array.create infoLength sentinelInfo
            let positions = Array.create positionLength sentinelPosition
            let outcome = LineShape.shapeInto plan font settings (ReadOnlySpan input) (Span infos) (Span positions) |> TailCalls.settle
            struct (outcome, infos, positions)
        let untouched (infos: GlyphInfo array) (positions: GlyphPosition array) (infoFrom: int) (positionFrom: int) =
            infos[infoFrom..] |> Array.forall (fun info -> info = sentinelInfo) && positions[positionFrom..] |> Array.forall (fun position -> position = sentinelPosition)
        match expected with
        | Error error ->
            let struct (outcome, infos, positions) = attempt (count + 4) (count + 4)
            match outcome with
            | IntoRefused refused -> sameResult (Error error) (Error refused) && untouched infos positions 0 0
            | IntoShaped _ | IntoTooShort _ -> false
        | Ok shaped ->
            let infoCount = shaped.Infos.Length
            let positionCount = shaped.Positions.Length
            let needed = max infoCount positionCount
            let struct (roomy, roomyInfos, roomyPositions) = attempt (needed + 4) (needed + 4)
            let roomyAgrees =
                match roomy with
                | IntoShaped (c, p, random, successful) ->
                    c = infoCount && p = positionCount && random = shaped.RandomState && successful = shaped.Successful
                    && roomyInfos[0 .. c - 1] = shaped.Infos.AsSpan().ToArray() && roomyPositions[0 .. p - 1] = shaped.Positions.AsSpan().ToArray()
                    && untouched roomyInfos roomyPositions c p
                | IntoTooShort _ | IntoRefused _ -> false
            let tightAgrees =
                let struct (tight, tightInfos, _) = attempt needed needed
                match tight with
                | IntoShaped (c, p, _, _) -> c = infoCount && p = positionCount && tightInfos[0 .. c - 1] = shaped.Infos.AsSpan().ToArray()
                | IntoTooShort _ | IntoRefused _ -> false
            let shortAgrees =
                match needed with
                | 0 -> true
                | _ ->
                    let struct (short, shortInfos, shortPositions) = attempt (needed - 1) (needed + 4)
                    match short with
                    | IntoTooShort asked -> asked = needed && untouched shortInfos shortPositions 0 0
                    | IntoShaped _ | IntoRefused _ -> false
            let aliasAgrees =
                let room = Array.append input (Array.create (needed + 4) sentinelInfo)
                let positions = Array.create (needed + 4) sentinelPosition
                let outcome = LineShape.shapeInto plan font settings (ReadOnlySpan(room, 0, count)) (Span room) (Span positions) |> TailCalls.settle
                match outcome with
                | IntoShaped (c, _, _, _) -> c = infoCount && room[0 .. c - 1] = shaped.Infos.AsSpan().ToArray()
                | IntoTooShort _ | IntoRefused _ -> false
            let shortPositionsAgrees =
                match needed with
                | 0 -> true
                | _ ->
                    let struct (short, shortInfos, shortPositions) = attempt (needed + 4) (needed - 1)
                    match short with
                    | IntoTooShort asked -> asked = needed && untouched shortInfos shortPositions 0 0
                    | IntoShaped _ | IntoRefused _ -> false
            let maskedAgrees =
                let masked = input |> Array.mapi (fun at info -> { info with Mask = uint32 (at + 1); Var1 = 7u; Var2 = uint32 at })
                let maskedRun = { run with Infos = ImmutableArray.CreateRange masked }
                let struct (expectedMasked, _) = Shape.shapeCached PlanCache.empty font case.Features case.Shapers maskedRun
                let infos = Array.create (count + 4 * (count + 4)) sentinelInfo
                let positions = Array.create (count + 4 * (count + 4)) sentinelPosition
                let outcome = LineShape.shapeInto plan font settings (ReadOnlySpan masked) (Span infos) (Span positions) |> TailCalls.settle
                match expectedMasked, outcome with
                | Ok maskedShaped, IntoShaped (c, p, _, successful) ->
                    c = maskedShaped.Infos.Length && p = maskedShaped.Positions.Length && successful = maskedShaped.Successful
                    && infos[0 .. c - 1] = maskedShaped.Infos.AsSpan().ToArray() && positions[0 .. p - 1] = maskedShaped.Positions.AsSpan().ToArray()
                | Error error, IntoRefused refused -> sameResult (Error error) (Error refused)
                | Ok _, IntoTooShort _ | Ok _, IntoRefused _ | Error _, IntoShaped _ | Error _, IntoTooShort _ -> false
            roomyAgrees && tightAgrees && shortAgrees && shortPositionsAgrees && aliasAgrees && maskedAgrees

/// <summary>Whether the shaping entry for arrays of the caller agrees on every line of a document.</summary>
/// <param name="case">The document.</param>
/// <returns>True when every line agrees.</returns>
let private intoEquals (case: Case) =
    Seq.forall (intoAgrees case) (seq { 0 .. case.Lines.Length - 1 })

/// <summary>The properties of the document entry points.</summary>
[<Tests>]
let tests =
    testList "line shape" [
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "the document entry equals shapeCached line by line, every script, encoding, flag and budget"
            (Prop.forAll (Arb.fromGen (caseGen ranges [| 0; 1; 2; 3; 4 |])) equalsReference)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "the document entry equals shapeCached line by line on Latin text, with and without layout"
            (Prop.forAll (Arb.fromGen (caseGen latinRanges [| 0; 0; 1; 4 |])) equalsReference)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "the document entry equals shapeCached line by line on ligature-heavy Latin text with kerning, with layout"
            (Prop.forAll (Arb.fromGen (caseGenOf ligatureLineGen [| 0; 0; 0; 0; 1; 4 |])) equalsReference)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "the document entry equals shapeCached line by line on Latin text, digits and punctuation in complex-script faces, with a word of another script now and then"
            (Prop.forAll (Arb.fromGen (caseGenOf mixedLineGen [| 3; 5; 6; 7; 8; 2; 3; 3; 0; 1; 4 |])) equalsReference)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "the document entry equals shapeCached line by line on lines of every script in the complex-script faces"
            (Prop.forAll (Arb.fromGen (caseGen ranges [| 5; 6; 7; 8 |])) equalsReference)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "Shape.shapeUtf8 equals decoding, guessing and shapeCached line by line, every script, flag and budget"
            (Prop.forAll (Arb.fromGen (caseGen ranges [| 0; 1; 2; 3; 4; 5; 6; 7; 8 |])) utf8EntryEquals)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "Shape.shapeUtf8 equals decoding, guessing and shapeCached on ligature-heavy Latin text with kerning, with layout"
            (Prop.forAll (Arb.fromGen (caseGenOf ligatureLineGen [| 0; 0; 1; 4 |])) utf8EntryEquals)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "shaping into arrays of the caller equals shapeCached, every script, flag, budget and array length, and the array the glyphs are in"
            (Prop.forAll (Arb.fromGen (caseGen ranges [| 0; 1; 2; 3; 4; 5; 6; 7; 8 |])) intoEquals)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "shaping into arrays of the caller equals shapeCached on ligature-heavy Latin text with kerning, with layout, every budget"
            (Prop.forAll (Arb.fromGen (caseGenOf ligatureLineGen [| 0; 0; 1; 4 |])) intoEquals)
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "shaping into arrays of the caller equals shapeCached on Latin text in complex-script faces"
            (Prop.forAll (Arb.fromGen (caseGenOf mixedLineGen [| 3; 5; 6; 7; 8; 2; 0; 1; 4 |])) intoEquals)
        testCase "ligature lines with kerning equal shapeCached under every operation budget up to 300 and the flags that change the tails" (fun () ->
            let texts = [| "the first official office staff were fine but the flowers fluffed after a shift"; "AV To fi Wa office Ty ffl fj ft P. r, fi"; "fi fi fi fi"; "fi"; "ffi 1 fl"; "1 fi 2 fi 3"; "fl fl fl fl fl fl"; "To fi" |]
            let flagChoices = [| BufferFlags.Default; BufferFlags.ProduceSafeToInsertTatweel; BufferFlags.ProduceUnsafeToConcat; BufferFlags.Bot ||| BufferFlags.Eot |]
            let budgets = Array.append [| 0 .. 300 |] [| 400; 600; 1000; GlyphRun.MaxOpsDefault |]
            for text in texts do
                let bytes = System.Text.Encoding.ASCII.GetBytes text
                for flags in flagChoices do
                    for budget in budgets do
                        for level in [| ClusterLevel.MonotoneGraphemes; ClusterLevel.Characters |] do
                            let case =
                                { FontIndex = 0
                                  Features = ImmutableArray<Feature>.Empty
                                  Shapers = ValueNone
                                  Settings = { GlyphRun.empty with Flags = flags; MaxOps = budget; ClusterLevel = level }
                                  Language = Language.cLocale
                                  Body = Utf8Body bytes
                                  Lines = [| struct (0, bytes.Length) |]
                                  Degree = 1
                                  Threshold = 0
                                  Warm = false }
                            Expect.isTrue (equalsReference case) $"{text} flags {flags} budget {budget} level {level}")
        testCase "long ligature lines with kerning equal shapeCached under every operation budget up to 1,500, where the substitution spends part of it" (fun () ->
            let texts =
                [| "AVfi To fi Wa office Ty ffl fj ft P. r, fi AVfi To fi Wa office Ty ffl fj ft P. r, fi"
                   "the first official office staff were fine but the flowers fluffed after a shift AV To"
                   "fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi"
                   "ToToToToToToToTo fi fi fi fi fi AVAVAVAV ffi ffl ff"
                   "fi AV" |]
            for text in texts do
                let bytes = System.Text.Encoding.ASCII.GetBytes text
                for flags in [| BufferFlags.Default; BufferFlags.ProduceUnsafeToConcat |] do
                    for budget in [| 0 .. 1500 |] do
                        let case =
                            { FontIndex = 0
                              Features = ImmutableArray<Feature>.Empty
                              Shapers = ValueNone
                              Settings = { GlyphRun.empty with Flags = flags; MaxOps = budget }
                              Language = Language.cLocale
                              Body = Utf8Body bytes
                              Lines = [| struct (0, bytes.Length) |]
                              Degree = 1
                              Threshold = 0
                              Warm = false }
                        Expect.isTrue (equalsReference case) $"{text} flags {flags} budget {budget}")
        testCase "ligature-heavy documents have many lines the substitution engine shortens, and the rest take other routes" (fun () ->
            let sample = Gen.sample 300 (caseGenOf ligatureLineGen [| 0 |]) |> List.ofSeq
            let outcomes =
                sample
                |> List.collect (fun case ->
                    let struct (document, _) = shapeLines case PlanCache.empty
                    let runs = case.Lines |> Array.map (runOf case)
                    [ for index in 0 .. document.Lines.Length - 1 -> struct (document.Lines[index], runs[index].Infos.Length) ])
            let shrunk = outcomes |> List.filter (fun (struct (outcome, count)) -> match outcome with | Arena (_, length) -> length < count | Alone _ -> false | Refused _ -> false)
            Expect.isGreaterThan shrunk.Length 500 "lines in which a ligature formed")
        testCase "the generated documents reach every kind of line: shrunk, grown, refused, empty and detached" (fun () ->
            let sample = Gen.sample 400 (caseGen ranges [| 0; 1; 2; 3; 4 |]) |> List.ofSeq
            let outcomes =
                sample
                |> List.collect (fun case ->
                    let struct (document, _) = shapeLines case PlanCache.empty
                    let runs = case.Lines |> Array.map (runOf case)
                    [ for index in 0 .. document.Lines.Length - 1 -> struct (document.Lines[index], runs[index].Infos.Length) ])
            let shrunk = outcomes |> List.filter (fun (struct (outcome, count)) -> match outcome with | Arena (_, length) -> length < count | Alone _ -> false | Refused _ -> false)
            let grown = outcomes |> List.filter (fun (struct (outcome, count)) -> match outcome with | Alone line -> line.LineInfos.Length > count | Arena _ -> false | Refused _ -> false)
            let refused = outcomes |> List.filter (fun (struct (outcome, _)) -> match outcome with | Refused _ -> true | Arena _ -> false | Alone _ -> false)
            let empty = outcomes |> List.filter (fun (struct (outcome, _)) -> match outcome with | Arena (_, 0) -> true | Arena _ -> false | Alone _ -> false | Refused _ -> false)
            Expect.isGreaterThan shrunk.Length 0 "lines that shrink"
            Expect.isGreaterThan grown.Length 0 "lines that grow past their window"
            Expect.isGreaterThan refused.Length 0 "lines that cannot be shaped"
            Expect.isGreaterThan empty.Length 20 "empty lines")
        testCase "an empty document gives an empty document and keeps the cache" (fun () ->
            let font = fonts.Value[0]
            let struct (document, cache) = LineShape.utf8 Shape.AllCores PlanCache.empty font ImmutableArray.Empty ValueNone Language.cLocale GlyphRun.empty ReadOnlyMemory<byte>.Empty ImmutableArray.Empty
            Expect.equal document.Lines.Length 0 "no lines"
            Expect.equal document.ArenaInfos.Length 0 "no glyphs"
            Expect.equal cache.Entries.Length 0 "no plans")
        testCase "a plain Latin line is two windows and nothing else" (fun () ->
            let font = fonts.Value[1]
            let text = System.Text.Encoding.ASCII.GetBytes "hello world\nsecond line\n"
            let lines = ImmutableArray.Create (struct (0, 11), struct (12, 11))
            let struct (document, _) = LineShape.utf8 1 PlanCache.empty font ImmutableArray.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) lines
            Expect.equal document.ArenaInfos.Length 22 "one array of infos for the document"
            Expect.equal document.Lines[0] (Arena (0, 11)) "the first window"
            Expect.equal document.Lines[1] (Arena (11, 11)) "the second window")
    ]
