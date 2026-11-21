module Farfbuzz.Diff.LayoutGens

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>A script and a language with their OpenType tag lists.</summary>
[<Struct>]
type ScriptLanguage =
    { Script: Script
      Language: Language
      ScriptTags: ImmutableArray<Tag>
      LanguageTags: ImmutableArray<Tag> }

/// <summary>Every script literal crossed with twelve languages, with the OtTag-derived tag lists.</summary>
/// <returns>The script and language pairs.</returns>
let scriptsAndLanguages () : ImmutableArray<ScriptLanguage> =
    let languages =
        [ "zh-hant"; "dv"; "sr"; "en"; "ar"; "fa"; "ur"; "hi"; "bn"; "ta"; "th"; "ja" ]
        |> List.map Language.ofString
    let pairs =
        Scripts.all
        |> Seq.collect (fun script ->
            languages
            |> List.map (fun language ->
                let struct (scriptTags, languageTags) = OtTag.tagsFromScriptAndLanguage script language 4 4
                { Script = script
                  Language = language
                  ScriptTags = scriptTags
                  LanguageTags = languageTags }))
        |> Seq.toArray
    ImmutableArray.CreateRange pairs

/// <summary>Collects the glyphs a lookup's coverage accepts.</summary>
/// <param name="subtables">The lookup's subtables.</param>
/// <param name="index">The next subtable.</param>
/// <param name="acc">The glyphs so far.</param>
/// <returns>The covered glyphs.</returns>
let rec internal lookupCoverage (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) (index: int) (acc: ImmutableArray<uint32>) =
    match index < subtables.Length with
    | false -> acc
    | true ->
        let glyphs =
            match subtables[index].Subtable with
            | SingleDelta (coverage, _, _)
            | SingleList (coverage, _)
            | Multiple (coverage, _)
            | Alternate (coverage, _)
            | LigatureSubst (coverage, _, _) -> Coverage.glyphs coverage
            | SubstContext (GlyphRules (coverage, _)) -> Coverage.glyphs coverage
            | SubstContext (ClassRules (coverage, _, _, _)) -> Coverage.glyphs coverage
            | SubstContext (CoverageRule (coverages, _)) ->
                match coverages.IsEmpty with
                | true -> ImmutableArray.Empty
                | false -> Coverage.glyphs coverages[0]
            | SubstChainContext (ChainGlyphRules (coverage, _)) -> Coverage.glyphs coverage
            | SubstChainContext (ChainClassRules (coverage, _, _, _, _, _)) -> Coverage.glyphs coverage
            | SubstChainContext (ChainCoverageRule (_, input, _, _)) ->
                match input.IsEmpty with
                | true -> ImmutableArray.Empty
                | false -> Coverage.glyphs input[0]
            | ReverseChainSingle (coverage, _, _, _) -> Coverage.glyphs coverage
            | SubstPending _ -> ImmutableArray.Empty
        lookupCoverage subtables (index + 1) (Sealed.appendSpan (acc.AsSpan()) (glyphs.AsSpan()))

/// <summary>Generates a single, multiple or alternate GSUB lookup index and 1-4 glyphs drawn from its coverage
/// and from random glyph ids.</summary>
/// <param name="font">The test font.</param>
/// <returns>The generator.</returns>
let lookupInputs (font: TestFont) =
    gen {
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)
        let gsub = (OtFace.create face).Gsub.Value
        let candidates =
            [| for lookup in 0 .. gsub.Lookups.Length - 1 do
                   match gsub.Lookups[lookup].Value.LookupType with
                   | 1 | 2 | 3 -> yield lookup
                   | _ -> () |]
        match candidates.Length with
        | 0 -> return struct (-1, [||])
        | _ ->
            let! lookup = Gen.elements candidates
            let covered = lookupCoverage gsub.Lookups[lookup].Value.Subtables 0 ImmutableArray.Empty
            let glyphCount = face.GlyphCount
            let! len = Gen.choose (1, 4)
            let! glyphs =
                Gen.arrayOfLength len
                    (match covered.Length > 0 with
                     | true -> Gen.frequency [ covered.Length, Gen.elements (covered.AsSpan().ToArray()); 1, Gen.choose (0, max 0 (glyphCount - 1)) |> Gen.map uint32 ]
                     | false -> Gen.choose (0, max 0 (glyphCount - 1)) |> Gen.map uint32)
            return struct (lookup, glyphs)
    }
