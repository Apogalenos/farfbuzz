module Farfbuzz.Diff.ComplexShapers.HangulKhmerDiff

open System
open System.Collections.Immutable
open System.IO
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Diff
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.ComplexShapers.HebrewThaiDiff

/// <summary>The cases generated per script on the corpus faces, the least the work asks for.</summary>
[<Literal>]
let internal CasesPerScript = 20_000

/// <summary>The fewest cases one variant of a face gets.</summary>
[<Literal>]
let internal CasesPerVariantMinimum = 500

/// <summary>The cases each synthetic font gets.</summary>
[<Literal>]
let internal CasesPerSynthetic = 2_500

/// <summary>The options of the Hangul differentials: each cluster level, each direction, a language, user features that
/// switch the jamo features and <c>calt</c> off, the buffer flags of the beginning and end of the text and of the default
/// ignorables, and no script, which HarfBuzz guesses.</summary>
let private hangulOptions : string array =
    [| "--script=Hang --direction=ltr --cluster-level=0"; "--script=Hang --direction=ltr --cluster-level=1"
       "--script=Hang --direction=ltr --cluster-level=2"; "--script=Hang --direction=ltr --cluster-level=3"
       "--script=Hang --direction=rtl --cluster-level=1"; "--script=Hang --language=ko"; "--script=Hang --features=-calt"
       "--script=Hang --features=ljmo=0,tjmo=0"; "--script=Hang --features=vjmo[1:3]=0"; "--script=Hang --direction=ttb"
       "--script=Hang --features=-calt[1:3]"; "--script=Hang --features=kern[1:3]=0,ccmp[0:2]=0,locl[2:4]=0"; "--script=Hang --features=-kern,-ccmp[1:4]"
       "--script=Hang --bot --eot"; "--script=Hang --remove-default-ignorables --cluster-level=2"; "--script=Hang --preserve-default-ignorables --cluster-level=3"; String.Empty |]

/// <summary>The options of the Khmer differentials.</summary>
let private khmerOptions : string array =
    [| "--script=Khmr --direction=ltr --cluster-level=0"; "--script=Khmr --direction=ltr --cluster-level=1"
       "--script=Khmr --direction=ltr --cluster-level=2"; "--script=Khmr --direction=ltr --cluster-level=3"
       "--script=Khmr --direction=rtl --cluster-level=1"; "--script=Khmr --language=km"; "--script=Khmr --features=liga"
       "--script=Khmr --features=-clig"; "--script=Khmr --features=pref=0,blwf=0"; "--script=Khmr --features=abvs[1:4]=0"; "--script=Khmr --direction=ttb"
       "--script=Khmr --bot --eot"; "--script=Khmr --remove-default-ignorables --cluster-level=2"; "--script=Khmr --preserve-default-ignorables --cluster-level=3"; String.Empty |]

/// <summary>A generator of one code point drawn from the candidates a face maps, and now and then from all of them.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <param name="candidates">The candidates.</param>
/// <returns>The generator.</returns>
let internal pick (covered: Set<uint32>) (candidates: uint32 list) : Gen<uint32> =
    let mapped = candidates |> List.filter covered.Contains |> List.toArray
    let all = List.toArray candidates
    match mapped.Length with
    | 0 -> Gen.elements all
    | _ -> Gen.frequency [ 4, Gen.elements mapped; 1, Gen.elements all ]

/// <summary>Builds a text from a generator of tokens: one to <paramref name="most"/> tokens, each a list of code points.</summary>
/// <param name="most">The most tokens.</param>
/// <param name="token">The token generator.</param>
/// <returns>The generator of texts.</returns>
let internal textOf (most: int) (token: Gen<uint32 list>) : Gen<uint32 array> =
    gen {
        let! count = Gen.choose (1, most)
        let! tokens = Gen.listOfLength count token
        return tokens |> List.concat |> List.toArray
    }

let internal latin = spanning [ 0x41u, 0x5Au; 0x61u, 0x7Au; 0x30u, 0x39u; 0x20u, 0x20u ]

/// <summary>The generator of Hangul texts for a face: syllables made of jamo in valid and broken sequences, old jamo,
/// precomposed syllables alone and followed by a trailing consonant, tone marks before and after, dotted circles and Latin.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private hangulText (covered: Set<uint32>) : Gen<uint32 array> =
    let pick = pick covered
    let leading = pick (spanning [ 0x1100u, 0x1112u ])
    let oldLeading = pick (spanning [ 0x1113u, 0x115Fu; 0xA960u, 0xA97Cu ])
    let vowel = pick (spanning [ 0x1161u, 0x1175u ])
    let oldVowel = pick (0x1160u :: spanning [ 0x1176u, 0x11A7u; 0xD7B0u, 0xD7C6u ])
    let trailing = pick (spanning [ 0x11A8u, 0x11C2u ])
    let oldTrailing = pick (spanning [ 0x11C3u, 0x11FFu; 0xD7CBu, 0xD7FBu ])
    let syllable = pick (spanning [ 0xAC00u, 0xD7A3u ])
    let leadingVowel = pick [ for index in 0u .. 18u * 21u -> 0xAC00u + index * 28u ]
    let any = Gen.oneof [ leading; oldLeading; vowel; oldVowel; trailing; oldTrailing ]
    let token =
        Gen.frequency
            [ 24, gen { let! l = leading in let! v = vowel in return [ l; v ] }
              16, gen { let! l = leading in let! v = vowel in let! t = trailing in return [ l; v; t ] }
              6, gen { let! l = Gen.oneof [ leading; oldLeading ] in let! v = Gen.oneof [ vowel; oldVowel ] in let! t = oldTrailing in return [ l; v; t ] }
              4, gen { let! l = oldLeading in let! v = Gen.oneof [ vowel; oldVowel ] in return [ l; v ] }
              14, gen { let! s = syllable in return [ s ] }
              6, gen { let! s = leadingVowel in let! t = trailing in return [ s; t ] }
              3, gen { let! s = syllable in let! t = Gen.oneof [ trailing; oldTrailing ] in return [ s; t ] }
              3, gen { let! s = leadingVowel in let! t = oldTrailing in return [ s; t ] }
              6, gen { let! j = any in return [ j ] }
              9, gen { let! t = pick [ 0x302Eu; 0x302Fu ] in return [ t ] }
              4, gen { let! t = Gen.elements [ 0x302Eu; 0x302Fu ] in let! s = syllable in return [ t; s ] }
              5, gen { let! c = pick latin in return [ c ] }
              2, Gen.constant [ 0x25CCu ] ]
    textOf 6 token

/// <summary>The generator of Khmer texts for a face: consonant clusters with coengs, ro, registers, vowels and signs,
/// broken clusters that need a dotted circle, joiners and Latin.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private khmerText (covered: Set<uint32>) : Gen<uint32 array> =
    let pick = pick covered
    let consonant = pick (spanning [ 0x1780u, 0x17A2u ])
    let ro = pick [ 0x179Au ]
    let independent = pick (spanning [ 0x17A3u, 0x17B3u ])
    let preVowel = pick [ 0x17C1u; 0x17C2u; 0x17C3u ]
    let split = pick [ 0x17BEu; 0x17BFu; 0x17C0u; 0x17C4u; 0x17C5u ]
    let aboveVowel = pick (spanning [ 0x17B7u, 0x17BAu ])
    let belowVowel = pick [ 0x17BBu; 0x17BCu; 0x17BDu ]
    let postVowel = pick [ 0x17B6u; 0x17C7u; 0x17C8u ]
    let vowel = Gen.oneof [ preVowel; split; aboveVowel; belowVowel; postVowel ]
    let register = pick [ 0x17C9u; 0x17CAu ]
    let sign = pick (spanning [ 0x17C6u, 0x17C6u; 0x17CBu, 0x17D1u; 0x17D3u, 0x17D3u; 0x17DDu, 0x17DDu ])
    let coeng = pick [ 0x17D2u ]
    let joiner = Gen.elements [ 0x200Cu; 0x200Du ]
    let coengConsonant = gen { let! c = Gen.frequency [ 3, consonant; 2, ro; 1, independent ] in return [ 0x17D2u; c ] }
    let maybe (probability: int) (item: Gen<uint32 list>) = Gen.frequency [ probability, item; 10 - probability, Gen.constant [] ]
    let cluster =
        gen {
            let! baseGlyph = Gen.frequency [ 8, consonant; 2, ro; 1, independent ]
            let! coengs =
                Gen.frequency
                    [ 5, Gen.constant []
                      4, coengConsonant
                      2, gen { let! a = coengConsonant in let! b = coengConsonant in return a @ b }
                      1, gen { let! a = coengConsonant in let! b = coengConsonant in let! c = coengConsonant in return a @ b @ c } ]
            let! joined = maybe 2 (gen { let! j = joiner in return [ j ] })
            let! shift = maybe 3 (gen { let! r = register in return [ r ] })
            let! vowels = Gen.frequency [ 4, Gen.constant []; 4, gen { let! v = vowel in return [ v ] }; 2, gen { let! a = vowel in let! b = vowel in return [ a; b ] } ]
            let! signs = maybe 3 (gen { let! s = sign in return [ s ] })
            return [ baseGlyph ] @ coengs @ joined @ shift @ vowels @ signs
        }
    let token =
        Gen.frequency
            [ 30, cluster
              4, gen { let! v = vowel in return [ v ] }
              3, gen { let! s = sign in return [ s ] }
              3, gen { let! c = coeng in return [ c ] }
              3, gen { let! c = coeng in let! k = consonant in return [ c; k ] }
              3, gen { let! k = consonant in let! c = coeng in return [ k; c ] }
              2, gen { let! j = joiner in return [ j ] }
              2, gen { let! k = consonant in let! j = joiner in let! v = vowel in return [ k; j; v ] }
              2, gen { let! r = register in return [ r ] }
              2, gen { let! t = split in let! k = consonant in return [ k; t ] }
              4, gen { let! c = pick (0x25CCu :: latin) in return [ c ] }
              2, gen { let! d = pick (spanning [ 0x17E0u, 0x17E9u; 0x17D4u, 0x17DAu ]) in return [ d ] } ]
    textOf 5 token

/// <summary>Reads the code points of a face with the glyph each maps to, through Farfbuzz.</summary>
/// <param name="source">The face.</param>
/// <returns>The code points and their glyphs.</returns>
let private mappingOf (source: TestFont) : (uint32 * uint32) list =
    let face = Face.create (ReadOnlyMemory (File.ReadAllBytes source.Path)) (uint32 source.Index)
    let font = Font.create face
    TestFonts.coverage source
    |> Array.toList
    |> List.choose (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome glyph -> Some (codepoint, glyph) | ValueNone -> None)

/// <summary>Rebuilds a single-face sfnt with some tables replaced or added.</summary>
/// <param name="bytes">The font bytes.</param>
/// <param name="replacements">The tables to put in, each with its tag.</param>
/// <returns>The new font bytes, the tables in tag order with a correct directory.</returns>
let internal withTables (bytes: byte array) (replacements: (uint32 * byte array) list) : byte array =
    let count = u16 bytes 4
    let existing =
        [ for index in 0 .. count - 1 ->
            let record = 12 + index * 16
            u32 bytes record, Array.sub bytes (int (u32 bytes (record + 8))) (int (u32 bytes (record + 12))) ]
    let kept = existing |> List.filter (fun (tag, _) -> not (replacements |> List.exists (fun (replaced, _) -> replaced = tag)))
    let tables = kept @ replacements |> List.sortBy fst
    let padded (data: byte array) = Array.append data (Array.zeroCreate ((4 - data.Length % 4) % 4))
    let total = tables.Length
    let entrySelector = int (Math.Log2 (float total))
    let searchRange = (1 <<< entrySelector) * 16
    let start = 12 + total * 16
    let offsets = tables |> List.scan (fun offset (_, data) -> offset + (padded data).Length) start
    let directory =
        List.map2 (fun (tag, data: byte array) offset -> Array.concat [ be32 tag; be32 0u; be32 (uint32 offset); be32 (uint32 data.Length) ]) tables (List.take total offsets)
    Array.concat ([ Array.sub bytes 0 4; be16 total; be16 searchRange; be16 entrySelector; be16 (total * 16 - searchRange) ] @ directory @ (tables |> List.map (snd >> padded)))

/// <summary>The directory where the synthetic fonts are written.</summary>
let internal syntheticDirectory () : string =
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "synthetic")
    Directory.CreateDirectory directory |> ignore
    directory

/// <summary>Writes font bytes under <c>artifacts/synthetic</c>.</summary>
/// <param name="name">The file name.</param>
/// <param name="bytes">The font bytes.</param>
/// <returns>The face.</returns>
let internal written (name: string) (bytes: byte array) : TestFont =
    let target = Path.Combine(syntheticDirectory (), name)
    File.WriteAllBytes(target, bytes)
    { Path = target; Suite = "synthetic"; Index = 0 }

/// <summary>The tag of a table as the number the directory holds.</summary>
/// <param name="text">The four characters.</param>
/// <returns>The tag.</returns>
let internal tagOf (text: string) : uint32 = uint32 (Tag.ofString text)

/// <summary>Builds a coverage table of the format with a glyph array.</summary>
/// <param name="glyphs">The glyphs, ascending.</param>
/// <returns>The table.</returns>
let internal coverageTable (glyphs: uint32 list) : byte array =
    Array.concat [ be16 1; be16 glyphs.Length; glyphs |> List.map (fun glyph -> be16 (int glyph)) |> Array.concat ]

/// <summary>Builds a single substitution lookup of the format that adds a delta to every glyph of a coverage.</summary>
/// <param name="glyphs">The glyphs it covers.</param>
/// <param name="delta">The delta added to each.</param>
/// <returns>The lookup table.</returns>
let internal singleSubstitution (glyphs: uint32 list) (delta: int) : byte array =
    let coverage = coverageTable (glyphs |> List.distinct |> List.sort)
    let subtable = Array.concat [ be16 1; be16 6; be16 (delta &&& 0xFFFF); coverage ]
    Array.concat [ be16 1; be16 0; be16 1; be16 8; subtable ]

/// <summary>Builds a ligature substitution lookup of the format with one ligature of two glyphs for each first glyph and
/// second glyph.</summary>
/// <param name="firsts">The first glyphs.</param>
/// <param name="seconds">The second glyphs.</param>
/// <param name="ligature">The ligature glyph.</param>
/// <returns>The lookup table.</returns>
let internal ligatureSubstitution (firsts: uint32 list) (seconds: uint32 list) (ligature: uint32) : byte array =
    let firsts = firsts |> List.distinct |> List.sort
    let seconds = seconds |> List.distinct |> List.sort
    let joined (second: uint32) = Array.concat [ be16 (int ligature); be16 2; be16 (int second) ]
    let set =
        let header = 2 + 2 * seconds.Length
        Array.concat ([ be16 seconds.Length ] @ (seconds |> List.mapi (fun index _ -> be16 (header + 6 * index))) @ (seconds |> List.map joined))
    let header = 6 + 2 * firsts.Length
    let coverageAt = header + set.Length * firsts.Length
    let subtable =
        Array.concat (
            [ be16 1; be16 coverageAt; be16 firsts.Length ]
            @ (firsts |> List.mapi (fun index _ -> be16 (header + set.Length * index)))
            @ (firsts |> List.map (fun _ -> set))
            @ [ coverageTable firsts ])
    Array.concat [ be16 4; be16 0; be16 1; be16 8; subtable ]

/// <summary>Builds a GSUB table with one script whose default language system lists every feature, and one lookup list.</summary>
/// <param name="script">The script tag.</param>
/// <param name="features">The features, each with the lookups it names.</param>
/// <param name="lookups">The lookup tables, in order.</param>
/// <returns>The table.</returns>
let internal gsubTable (script: string) (features: (string * int list) list) (lookups: byte array list) : byte array =
    let sorted = features |> List.sortBy fst
    let ascii (text: string) = Encoding.ASCII.GetBytes text
    let langSys = Array.concat [ be16 0; be16 0xFFFF; be16 sorted.Length; [ 0 .. sorted.Length - 1 ] |> List.map be16 |> Array.concat ]
    let scriptTable = Array.concat [ be16 4; be16 0; langSys ]
    let scriptList = Array.concat [ be16 1; ascii script; be16 8; scriptTable ]
    let featureTables = sorted |> List.map (fun (_, indices) -> Array.concat [ be16 0; be16 indices.Length; indices |> List.map be16 |> Array.concat ])
    let featureHeader = 2 + 6 * sorted.Length
    let featureOffsets = featureTables |> List.scan (fun offset table -> offset + table.Length) featureHeader
    let featureList =
        Array.concat (
            [ be16 sorted.Length ]
            @ (List.map2 (fun (tag, _) offset -> Array.concat [ ascii tag; be16 offset ]) sorted (List.take sorted.Length featureOffsets))
            @ featureTables)
    let lookupHeader = 2 + 2 * lookups.Length
    let lookupOffsets = lookups |> List.scan (fun offset table -> offset + table.Length) lookupHeader
    let lookupList = Array.concat ([ be16 lookups.Length ] @ (List.take lookups.Length lookupOffsets |> List.map be16) @ lookups)
    let featureAt = 10 + scriptList.Length
    Array.concat [ be16 1; be16 0; be16 10; be16 featureAt; be16 (featureAt + featureList.Length); scriptList; featureList; lookupList ]

/// <summary>The source of the synthetic fonts: a face of 65,535 glyphs and no layout, whose cmap and tables are replaced.</summary>
let internal baseFace : Lazy<TestFont> = lazy (TestFonts.named "FDArrayTest65535.otf")

/// <summary>Gives each code point of a list a glyph of its own, in order from glyph 1.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The code points and glyphs.</returns>
let internal distinctMapping (codepoints: uint32 list) : (uint32 * uint32) list =
    codepoints |> List.distinct |> List.sort |> List.mapi (fun index codepoint -> codepoint, uint32 (index + 1))

/// <summary>Builds a synthetic font: the base face with a cmap of distinct glyphs and optional other tables.</summary>
/// <param name="name">The file name.</param>
/// <param name="mapping">The code points and glyphs.</param>
/// <param name="tables">The tables to add or replace.</param>
/// <returns>The face.</returns>
let internal syntheticFont (name: string) (mapping: (uint32 * uint32) list) (tables: (uint32 * byte array) list) : TestFont =
    let bytes = File.ReadAllBytes baseFace.Value.Path
    written name (withTables bytes ((tagOf "cmap", cmapOf mapping) :: tables))

/// <summary>The glyph of a code point in a mapping.</summary>
/// <param name="mapping">The mapping.</param>
/// <param name="codepoint">The code point.</param>
/// <returns>The glyph, none when unmapped.</returns>
let internal glyphOf (mapping: (uint32 * uint32) list) (codepoint: uint32) : uint32 option =
    mapping |> List.tryFind (fun (mapped, _) -> mapped = codepoint) |> Option.map snd

/// <summary>The glyphs of the code points of ranges a mapping has.</summary>
/// <param name="mapping">The mapping.</param>
/// <param name="ranges">The ranges.</param>
/// <returns>The glyphs.</returns>
let internal glyphsIn (mapping: (uint32 * uint32) list) (ranges: (uint32 * uint32) list) : uint32 list =
    mapping |> List.filter (fun (codepoint, _) -> ranges |> List.exists (fun (first, last) -> codepoint >= first && codepoint <= last)) |> List.map snd

/// <summary>The code points every synthetic Hangul font starts from.</summary>
let private hangulCodepoints : uint32 list =
    spanning [ 0x1100u, 0x11FFu; 0xA960u, 0xA97Fu; 0xD7B0u, 0xD7FFu; 0xAC00u, 0xD7A3u; 0x302Eu, 0x302Fu; 0x25CCu, 0x25CCu ] @ latin

/// <summary>The GSUB table of the synthetic Hangul fonts: <c>calt</c> over every jamo and the Latin letters, applied first,
/// then one lookup per jamo feature, each adding a delta of its own to the glyphs it covers.</summary>
/// <param name="mapping">The code points and glyphs of the font.</param>
/// <returns>The table.</returns>
let private hangulGsub (mapping: (uint32 * uint32) list) : byte array =
    let leading = glyphsIn mapping [ 0x1100u, 0x115Fu; 0xA960u, 0xA97Cu ]
    let vowels = glyphsIn mapping [ 0x1160u, 0x11A7u; 0xD7B0u, 0xD7C6u ]
    let trailing = glyphsIn mapping [ 0x11A8u, 0x11FFu; 0xD7CBu, 0xD7FBu ]
    let letters = glyphsIn mapping [ 0x41u, 0x5Au; 0x61u, 0x7Au ]
    gsubTable "hang" [ "calt", [ 0 ]; "ljmo", [ 1 ]; "vjmo", [ 2 ]; "tjmo", [ 3 ] ]
        [ singleSubstitution (leading @ vowels @ trailing @ letters) 20000
          singleSubstitution leading 5000
          singleSubstitution vowels 10000
          singleSubstitution trailing 15000 ]

/// <summary>Builds a legacy <c>kern</c> table of one horizontal format-0 subtable that kerns every ordered pair of a list of glyphs
/// by an amount that depends on the pair.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The table.</returns>
let private kernTable (glyphs: uint32 list) : byte array =
    let pairs = [ for left in glyphs do for right in glyphs -> left, right ] |> List.sort
    let count = pairs.Length
    let entrySelector = int (Math.Log2 (float count))
    let searchRange = 6 * (1 <<< entrySelector)
    let value (left: uint32, right: uint32) = -(50 + int ((left * 7u + right * 3u) % 200u))
    let entry (left: uint32, right: uint32) = Array.concat [ be16 (int left); be16 (int right); be16 (value (left, right) &&& 0xFFFF) ]
    let subtable =
        Array.concat ([ be16 0; be16 (14 + 6 * count); be16 1; be16 count; be16 searchRange; be16 entrySelector; be16 (6 * count - searchRange) ] @ (pairs |> List.map entry))
    Array.concat [ be16 0; be16 1; subtable ]

/// <summary>The synthetic Hangul fonts: a font with everything, one with the jamo features, and fonts that lack the
/// precomposed syllables, the jamo, the trailing consonants of syllables, a vowel, the dotted circle, or have a tone mark
/// with no advance, and one with a legacy kern table and no GPOS.</summary>
/// <returns>The fonts, each with its name.</returns>
let private hangulFonts () : (string * TestFont) list =
    let full = distinctMapping hangulCodepoints
    let without (drop: uint32 -> bool) = full |> List.filter (fun (codepoint, _) -> not (drop codepoint))
    let isSyllable (u: uint32) = u >= 0xAC00u && u <= 0xD7A3u
    let isJamo (u: uint32) = (u >= 0x1100u && u <= 0x11FFu) || (u >= 0xA960u && u <= 0xA97Fu) || (u >= 0xD7B0u && u <= 0xD7FFu)
    let withGsub (mapping: (uint32 * uint32) list) = [ tagOf "GSUB", hangulGsub mapping ]
    let lvOnly = without (fun u -> isSyllable u && (u - 0xAC00u) % 28u <> 0u)
    let sparse = without (fun u -> (isSyllable u && (u * 7u) % 5u = 0u) || (u >= 0x11A8u && u <= 0x11FFu && u % 2u = 0u))
    let noVowel = without (fun u -> (u >= 0x1161u && u <= 0x1175u && u % 3u = 0u) || (u >= 0x1160u && u <= 0x11A7u && u % 7u = 0u))
    let noSyllables = without isSyllable
    let noJamo = without isJamo
    let noCircle = without (fun u -> u = 0x25CCu)
    let zeroWidthGlyph = (glyphOf full 0x302Fu).Value
    let last = full |> List.map snd |> List.max
    let metrics =
        [ for glyph in 0u .. last -> (match glyph = zeroWidthGlyph with | true -> 0 | false -> 1000) ]
    let hmtx = metrics |> List.map (fun advance -> Array.concat [ be16 advance; be16 0 ]) |> Array.concat
    let hhea =
        let original = Face.referenceTable (Face.create (ReadOnlyMemory (File.ReadAllBytes baseFace.Value.Path)) 0u) Tags.Hhea |> fun table -> table.ToArray()
        Array.concat [ Array.sub original 0 34; be16 (int last + 1) ]
    [ "hangul-full", syntheticFont "hangul-full.otf" full []
      "hangul-jamo-features", syntheticFont "hangul-gsub.otf" full (withGsub full)
      "hangul-no-syllables", syntheticFont "hangul-no-syllables.otf" noSyllables (withGsub noSyllables)
      "hangul-no-jamo", syntheticFont "hangul-no-jamo.otf" noJamo (withGsub noJamo)
      "hangul-lv-only", syntheticFont "hangul-lv-only.otf" lvOnly (withGsub lvOnly)
      "hangul-sparse", syntheticFont "hangul-sparse.otf" sparse (withGsub sparse)
      "hangul-no-vowels", syntheticFont "hangul-no-vowels.otf" noVowel (withGsub noVowel)
      "hangul-no-dotted-circle", syntheticFont "hangul-no-circle.otf" noCircle (withGsub noCircle)
      "hangul-zero-width-tone", syntheticFont "hangul-zero-tone.otf" full ([ tagOf "hmtx", hmtx; tagOf "hhea", hhea ] @ withGsub full)
      "hangul-kern", syntheticFont "hangul-kern.otf" full [ tagOf "kern", kernTable (glyphsIn full [ 0x41u, 0x5Au; 0x61u, 0x7Au; 0x30u, 0x39u; 0x20u, 0x20u ]) ] ]

/// <summary>The code points every synthetic Khmer font starts from.</summary>
let private khmerCodepoints : uint32 list =
    spanning [ 0x1780u, 0x17FFu; 0x200Cu, 0x200Du; 0x25CCu, 0x25CCu ] @ latin

/// <summary>The GSUB table of the synthetic Khmer fonts: one lookup per feature of the shaper, each adding a delta of its
/// own to the glyphs of a group, the groups made to overlap so that the order of the lookups and the masks show.</summary>
/// <param name="mapping">The code points and glyphs of the font.</param>
/// <returns>The table.</returns>
let private khmerGsub (mapping: (uint32 * uint32) list) : byte array =
    let group (ranges: (uint32 * uint32) list) (remainder: uint32) =
        mapping |> List.filter (fun (codepoint, _) -> ranges |> List.exists (fun (first, last) -> codepoint >= first && codepoint <= last) && codepoint % 3u = remainder) |> List.map snd
    let consonants = [ 0x1780u, 0x17A2u ]
    let single (codepoints: uint32 list) = codepoints |> List.choose (glyphOf mapping)
    gsubTable "khmr"
        [ "locl", [ 0 ]; "ccmp", [ 1; 13 ]; "pref", [ 2 ]; "blwf", [ 3 ]; "abvf", [ 4 ]; "pstf", [ 5 ]; "cfar", [ 6 ]
          "pres", [ 7 ]; "abvs", [ 8 ]; "blws", [ 9 ]; "psts", [ 10 ]; "clig", [ 11 ]; "liga", [ 12 ] ]
        [ singleSubstitution (glyphsIn mapping [ 0x41u, 0x5Au; 0x30u, 0x39u ]) 30000
          singleSubstitution (glyphsIn mapping [ 0x17A3u, 0x17B3u ]) 31000
          singleSubstitution (single [ 0x17D2u; 0x179Au ]) 1000
          singleSubstitution (group consonants 2u) 2000
          singleSubstitution (group consonants 0u) 3000
          singleSubstitution (group consonants 1u) 4000
          singleSubstitution (glyphsIn mapping [ 0x17B7u, 0x17BAu ]) 5000
          singleSubstitution (glyphsIn mapping [ 0x17C1u, 0x17C3u ] @ single [ 0x17D2u ]) 6000
          singleSubstitution (glyphsIn mapping [ 0x17BEu, 0x17C0u ]) 7000
          singleSubstitution (glyphsIn mapping [ 0x17BBu, 0x17BDu ]) 8000
          singleSubstitution (glyphsIn mapping [ 0x17B6u, 0x17B6u; 0x17C7u, 0x17C8u ]) 9000
          singleSubstitution (glyphsIn mapping [ 0x17C6u, 0x17D1u ]) 10000
          singleSubstitution (group consonants 1u @ group consonants 2u) 11000
          ligatureSubstitution (glyphsIn mapping consonants) (glyphsIn mapping consonants) (List.head (glyphsIn mapping [ 0x41u, 0x41u ])) ]

/// <summary>The synthetic Khmer fonts: one with every feature, one with no layout table, one without the dotted circle,
/// one with half of the code points, and one without the vowel sign e that the split vowels decompose into.</summary>
/// <returns>The fonts, each with its name.</returns>
let private khmerFonts () : (string * TestFont) list =
    let full = distinctMapping khmerCodepoints
    let without (drop: uint32 -> bool) = full |> List.filter (fun (codepoint, _) -> not (drop codepoint))
    let withGsub (mapping: (uint32 * uint32) list) = [ tagOf "GSUB", khmerGsub mapping ]
    let noCircle = without (fun u -> u = 0x25CCu)
    let half = without (fun u -> u >= 0x1780u && u <= 0x17FFu && (u * 11u) % 7u < 3u)
    let noVowelE = without (fun u -> u = 0x17C1u)
    [ "khmer-features", syntheticFont "khmer-gsub.otf" full (withGsub full)
      "khmer-no-layout", syntheticFont "khmer-full.otf" full []
      "khmer-no-dotted-circle", syntheticFont "khmer-no-circle.otf" noCircle (withGsub noCircle)
      "khmer-half", syntheticFont "khmer-half.otf" half (withGsub half)
      "khmer-no-vowel-e", syntheticFont "khmer-no-e.otf" noVowelE (withGsub noVowelE) ]

/// <summary>The code points of a face as a set, cached.</summary>
/// <param name="font">The face.</param>
/// <returns>The set.</returns>
let internal coverageOf (font: TestFont) : Set<uint32> = covered font

/// <summary>The faces of the corpus that map at least one code point of the ranges.</summary>
/// <param name="ranges">The ranges.</param>
/// <returns>The faces.</returns>
let internal corpusFaces (ranges: (uint32 * uint32) list) : TestFont list =
    facesCovering (spanning ranges) 1

/// <summary>The Hangul faces of the corpus.</summary>
let private hangulFaces : Lazy<TestFont list> = lazy (corpusFaces [ 0x1100u, 0x11FFu; 0xAC00u, 0xD7A3u ])

/// <summary>The Khmer faces of the corpus.</summary>
let private khmerFaces : Lazy<TestFont list> = lazy (corpusFaces [ 0x1780u, 0x17FFu ])

/// <summary>The variants of a face every differential runs: the face with its layout, the face stripped of every layout
/// table, and the face with only its GPOS renamed out. Faces whose GPOS holds context lookups stay in: the positioning kernel
/// applies them.</summary>
/// <param name="font">The face.</param>
/// <returns>The variants and their names.</returns>
let internal variants (font: TestFont) : (string * TestFont) list =
    let gsubOnly =
        match withoutGpos font with
        | ValueSome copy -> [ "no GPOS", copy ]
        | ValueNone -> []
    [ "layout", font; "stripped", NoLayoutFonts.strip font ] @ gsubOnly

/// <summary>A copy of a face whose cmap lacks the precomposed syllables, with all its other tables: a font that has the
/// jamo and their features but no syllable to compose into.</summary>
/// <param name="font">The face.</param>
/// <returns>The copy, or none for a face of a collection or one that maps no syllable.</returns>
let private withoutSyllables (font: TestFont) : TestFont voption =
    let bytes = File.ReadAllBytes font.Path
    let plain = bytes.Length > 12 && u32 bytes 0 <> uint32 Tags.Collection && font.Index = 0
    let isSyllable (codepoint: uint32) = codepoint >= 0xAC00u && codepoint <= 0xD7A3u
    let mapping = mappingOf font
    match plain && mapping |> List.exists (fst >> isSyllable) with
    | false -> ValueNone
    | true ->
        let kept = mapping |> List.filter (fst >> isSyllable >> not) |> List.sortBy fst
        let name = Path.GetFileNameWithoutExtension font.Path |> nonNull
        ValueSome (written $"{name}-no-syllables.otf" (withCmap bytes (cmapOf kept)))

/// <summary>The variants of a Hangul face: those of every face, and the copy without the precomposed syllables.</summary>
/// <param name="font">The face.</param>
/// <returns>The variants and their names.</returns>
let private hangulVariants (font: TestFont) : (string * TestFont) list =
    variants font @ (match withoutSyllables font with | ValueSome copy -> [ "no syllables", copy ] | ValueNone -> [])

/// <summary>A sampled differential for each variant of each face, on texts of a generator built from the face's coverage.</summary>
/// <param name="prefix">The test name prefix.</param>
/// <param name="faces">The faces.</param>
/// <param name="variantsOf">The variants of a face.</param>
/// <param name="options">The option fields.</param>
/// <param name="textFor">The generator of texts for a face's coverage.</param>
/// <param name="total">The number of cases to reach over the variants.</param>
/// <returns>The tests.</returns>
let internal sampledTexts (prefix: string) (faces: TestFont list) (variantsOf: TestFont -> (string * TestFont) list) (options: string array) (textFor: Set<uint32> -> Gen<uint32 array>) (total: int) : Test list =
    let all = faces |> List.collect (fun font -> variantsOf font |> List.map (fun (kind, variant) -> font, kind, variant))
    let each = max CasesPerVariantMinimum ((total + all.Length - 1) / max 1 all.Length)
    Console.WriteLine $"{prefix}: {faces.Length} faces ({names faces}), {all.Length} variants, {each} cases each, {each * all.Length} cases"
    [ for font, kind, variant in all -> ShapeDiff.againstTexts $"{prefix} {label font} {kind}" variant.Path options (textFor (coverageOf font)) each ]

/// <summary>A sampled differential for each synthetic font, with the generator built from the font's own coverage.</summary>
/// <param name="prefix">The test name prefix.</param>
/// <param name="fonts">The fonts.</param>
/// <param name="options">The option fields.</param>
/// <param name="textFor">The generator of texts.</param>
/// <returns>The tests.</returns>
let internal sampledSynthetic (prefix: string) (fonts: (string * TestFont) list) (options: string array) (textFor: Set<uint32> -> Gen<uint32 array>) : Test list =
    let listed = fonts |> List.map fst |> String.concat " "
    Console.WriteLine $"{prefix}: {fonts.Length} synthetic fonts ({listed}), {CasesPerSynthetic} cases each, {fonts.Length * CasesPerSynthetic} cases"
    [ for name, font in fonts -> ShapeDiff.againstTexts $"{prefix} {name}" font.Path options (textFor (coverageOf font)) CasesPerSynthetic ]

/// <summary>The synthetic Hangul fonts, written once.</summary>
let private hangulSynthetic : Lazy<(string * TestFont) list> = lazy (hangulFonts ())

/// <summary>The synthetic Khmer fonts, written once.</summary>
let private khmerSynthetic : Lazy<(string * TestFont) list> = lazy (khmerFonts ())

/// <summary>The Hangul shaping differential: Farfbuzz against HarfBuzz on random Hangul text, on every corpus face that
/// maps some Hangul, each with its layout, without layout tables and without GPOS, and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let hangulTests () : Test =
    testList "P6.S16 hangul shape" (
        sampledTexts "hangul" hangulFaces.Value hangulVariants hangulOptions hangulText CasesPerScript
        @ sampledSynthetic "hangul synthetic" hangulSynthetic.Value hangulOptions hangulText)

/// <summary>The Khmer shaping differential: Farfbuzz against HarfBuzz on random Khmer text, on every corpus face that
/// maps some Khmer, each with its layout, without layout tables and without GPOS, and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let khmerTests () : Test =
    testList "P6.S20 khmer shape" (
        sampledTexts "khmer" khmerFaces.Value variants khmerOptions khmerText CasesPerScript
        @ sampledSynthetic "khmer synthetic" khmerSynthetic.Value khmerOptions khmerText)

/// <summary>The font and option pairs of the plan differential: each script spelling in each direction, and the face
/// stripped of its layout.</summary>
/// <param name="faces">The faces.</param>
/// <param name="scripts">The script options.</param>
/// <returns>The pairs.</returns>
let internal planCases (faces: TestFont list) (scripts: string list) : (string * string) list =
    [ for font in faces do
        for script in scripts do
            for direction in [ " --direction=ltr"; " --direction=rtl"; " --direction=ttb"; " --direction=btt" ] do
                yield font.Path, $"{script}{direction}"
        let stripped = NoLayoutFonts.strip font
        yield stripped.Path, $"{scripts.Head} --direction=ltr" ]

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for the
/// Hangul and Khmer faces and the synthetic fonts, in each direction.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    let hangul = hangulFaces.Value @ (hangulSynthetic.Value |> List.map snd)
    let khmer = khmerFaces.Value @ (khmerSynthetic.Value |> List.map snd)
    testList "P6.S16 P6.S20 plan" [
        PlanDiff.featuresAgainst "hangul plan" (planCases hangul [ "--script=Hang"; "--script=Hang --language=ko" ])
        PlanDiff.featuresAgainst "khmer plan" (planCases khmer [ "--script=Khmr"; "--script=Khmr --language=km" ])
    ]

/// <summary>What the trace differential compares of one glyph at a pause point: the glyph, its mask with the glyph flags, its
/// cluster, its syllable byte and its category.</summary>
type internal TracedGlyph = uint32 * uint32 * uint32 * byte * byte

/// <summary>The snapshots HarfBuzz takes at the Khmer pause points: the start of the reordering, the end of the insertion of
/// dotted circles, the end of the reordering, and around the whole GSUB table: its start and its end.</summary>
let private khmerMessages : string list =
    [ "start reordering khmer"; "end inserting dotted-circles"; "end reordering khmer"; "start table GSUB"; "end table GSUB" ]

/// <summary>Reads the glyphs of a HarfBuzz snapshot: its syllable is the high byte of <c>var1</c> and its category the third
/// byte of <c>var2</c>.</summary>
/// <param name="infos">The glyph infos.</param>
/// <returns>The glyphs.</returns>
let internal tracedOracle (infos: GlyphInfo array voption) : TracedGlyph array voption =
    infos |> ValueOption.map (Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster, byte (info.Var1 >>> 24), byte (info.Var2 >>> 16)))

/// <summary>Reads the glyphs of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyphs.</returns>
let internal tracedRun (run: GlyphRun) : TracedGlyph array =
    Array.init run.Infos.Length (fun index -> run.Infos[index].Codepoint, run.Infos[index].Mask, run.Infos[index].Cluster, run.Props[index].Syllable, run.Props[index].ShaperCategory)

/// <summary>Runs the stages of the plan's GSUB map up to the pause of the Khmer reordering, as <c>hb_ot_map_t::apply</c>
/// does, but for that pause's own function.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="run">The run.</param>
/// <returns>The run just before the reordering pause.</returns>
let rec private untilReorder (plan: OtShapePlan) (font: Font) stage first (run: GlyphRun) : GlyphRun =
    let current = plan.Map.GsubStages[stage]
    match current.Pause with
    | KhmerReorder -> OtLayout.substituteWindow plan font first current.LastLookup run
    | NoPause | ClearSyllables | KhmerSetupSyllables | IndicSetupSyllables | IndicInitialReordering | IndicFinalReordering
    | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder
    | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction ->
        match PauseKind.runsFunction current.Pause with
        | false -> untilReorder plan font (stage + 1) first run
        | true ->
            let windowed = OtLayout.substituteWindow plan font first current.LastLookup run
            untilReorder plan font (stage + 1) current.LastLookup (ShaperHooks.applyPause current.Pause plan font windowed)

/// <summary>Runs the pipeline of a request up to the substitution: the plan, the font and the run as the GSUB pass starts, with
/// its glyph classes synthesized when the plan asks.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>The font, the plan and the run.</returns>
let internal beforeSubstitution (fontFile: string) (request: ShapeRequest) : struct (Font * OtShapePlan * GlyphRun) =
    let font = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data fontFile)) 0u)
    let direction = match request.Direction with | ValueSome text -> Direction.ofString text | ValueNone -> Direction.Invalid
    let script = match request.Script with | ValueSome text -> Script.ofString text | ValueNone -> Scripts.Invalid
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan request.Text) 0 request.Text.Length GlyphRun.empty
        |> GlyphRun.withSegment { Direction = direction; Script = script; Language = Language.ofString request.Language }
        |> GlyphRun.withFlags (LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.BufferFlags> (uint32 request.Flags))
        |> GlyphRun.withClusterLevel (match request.ClusterLevel with | 1 -> ClusterLevel.MonotoneCharacters | 2 -> ClusterLevel.Characters | 3 -> ClusterLevel.Graphemes | _ -> ClusterLevel.MonotoneGraphemes)
        |> GlyphRun.guessSegmentProperties Language.cLocale
        |> TailCalls.settle
    match Execute.createPlan font run.SegmentProperties ImmutableArray.Empty ValueNone with
    | Error error -> failtest $"no plan: {error}"
    | Ok shapePlan ->
        let plan = ValueOption.get shapePlan.Ot
        let entered = GlyphRun.enter run
        let propped = OtProps.setUnicodePropsFused plan.Map.GlobalMask entered
        let native = OtProps.ensureNativeDirection (OtProps.formClusters (OtProps.insertDottedCircle font propped))
        let prepared = ShaperHooks.preprocess plan font native
        let rotated = OtSubstitute.rotateChars plan font plan.Props.Direction prepared
        let masked = OtSubstitute.setupMasks plan (Normalize.run plan font rotated)
        let recategorized = match plan.FallbackMarkPositioning with | true -> OtLayout.recategorizeMarks masked | false -> masked
        let started = OtSubstitute.substituteStart font (Normalize.mapGlyphsFast recategorized)
        let synthesized = match plan.FallbackGlyphClasses with | true -> OtProps.synthesizeGlyphClasses started | false -> started
        struct (font, plan, synthesized)

/// <summary>The snapshots Farfbuzz has at the Khmer pause points, by running the pipeline up to the substitution and the
/// stages up to and through the reordering pause.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>One snapshot per message of <see cref="khmerMessages"/>, none for a message HarfBuzz would not send.</returns>
let private farfbuzzTrace (fontFile: string) (request: ShapeRequest) : TracedGlyph array voption list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let tableSnapshots =
        match plan.ApplyMorx with
        | true -> [ ValueNone; ValueNone ]
        | false -> [ ValueSome (tracedRun synthesized); ValueSome (tracedRun (OtLayout.substitute plan font synthesized)) ]
    let khmerSnapshots =
        match plan.Shaper.Kind = KhmerShaper with
        | false -> [ ValueNone; ValueNone; ValueNone ]
        | true ->
            let before = untilReorder plan font 0 0 synthesized
            let struct (circled, inserted) = SyllableEdit.insertDottedCircles font 1uy KhmerMachine.DOTTEDCIRCLE ValueNone ValueNone before
            let after = ShaperHooks.applyPause KhmerReorder plan font before
            [ ValueSome (tracedRun before)
              (match inserted with | true -> ValueSome (tracedRun circled) | false -> ValueNone)
              ValueSome (tracedRun after) ]
    khmerSnapshots @ tableSnapshots

/// <summary>A trace differential on random Khmer text: the glyphs, clusters, masks, syllables and categories at each pause
/// point of the Khmer shaper agree with HarfBuzz's, so a difference is found at its stage.</summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="covered">The code points the face maps.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private khmerTrace (name: string) (fontFile: string) (covered: Set<uint32>) (count: int) : Test =
    Differential.sampled name (ShapeDiff.casesOfTexts fontFile khmerOptions (khmerText covered)) count
        (fun request -> farfbuzzTrace fontFile request)
        (fun request -> TraceDiff.traceMessages fontFile request khmerMessages |> List.map tracedOracle)

/// <summary>The Khmer trace differentials: at the start of the reordering (after the syllables are found), after the
/// dotted circles are inserted and at the end of the reordering, on the corpus Khmer faces with layout and on the
/// synthetic Khmer fonts.</summary>
/// <returns>The tests.</returns>
let khmerTraceTests () : Test =
    let corpus = khmerFaces.Value |> List.collect (fun font -> variants font |> List.map (fun (kind, variant) -> $"khmer trace {label font} {kind}", variant))
    let synthetic = khmerSynthetic.Value |> List.map (fun (name, font) -> $"khmer trace {name}", font)
    Console.WriteLine $"khmer trace: {khmerFaces.Value.Length} corpus faces ({names khmerFaces.Value}) in {corpus.Length} variants, {synthetic.Length} synthetic fonts, 1500 cases each, {(corpus.Length + synthetic.Length) * 1500} cases"
    testList "P6.S20 khmer trace" [ for name, font in corpus @ synthetic -> khmerTrace name font.Path (coverageOf font) 1500 ]
