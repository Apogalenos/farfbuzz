module Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format 12 cmap subtable mapping each code point to its own glyph.</summary>
/// <param name="mapping">The code point and glyph pairs, ascending by code point.</param>
/// <returns>The subtable bytes.</returns>
let private format12 (mapping: (uint32 * uint32) list) : byte array =
    let body = mapping |> List.map (fun (codepoint, glyph) -> Array.concat [ be32 codepoint; be32 codepoint; be32 glyph ]) |> Array.concat
    Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 mapping.Length); body ]

/// <summary>Builds a font that has a cmap and nothing else, mapping the given code points to the given glyphs.</summary>
/// <param name="mapping">The code point and glyph pairs.</param>
/// <returns>The font.</returns>
let fontMapping (mapping: (uint32 * uint32) list) : Font =
    let sorted = mapping |> List.sortBy fst
    let subtable = format12 sorted
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 10u; be32 12u; subtable ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap ])) 0u |> Font.create

/// <summary>Compiles the plan of a shaper for a script with empty capabilities and the empty map, as a font with no
/// GSUB script would give it.</summary>
/// <param name="kind">The shaper kind.</param>
/// <param name="script">The script.</param>
/// <returns>The plan.</returns>
let internal planFor (kind: ShaperKind) (script: Script) : OtShapePlan =
    let props = { Direction = Direction.Ltr; Script = script; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty kind

/// <summary>Builds a run from code points and sets its Unicode properties, one cluster per code point.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let runOf (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
    |> OtProps.setUnicodeProps 0xFFFFFFFFu
    |> TailCalls.settle

/// <summary>Reads the code points of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points.</returns>
let codepointsOf (run: GlyphRun) : uint32 array =
    [| for info in run.Infos -> info.Codepoint |]

/// <summary>Reads the clusters of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The clusters.</returns>
let clustersOf (run: GlyphRun) : uint32 array =
    [| for info in run.Infos -> info.Cluster |]

/// <summary>Builds a run from code points at a cluster level, with a global mask, one cluster per code point.</summary>
/// <param name="level">The cluster level.</param>
/// <param name="mask">The global mask every glyph starts with.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let runWith (level: ClusterLevel) (mask: uint32) (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
    |> GlyphRun.withClusterLevel level
    |> OtProps.setUnicodeProps mask
    |> TailCalls.settle

/// <summary>Reads the masks of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The masks.</returns>
let masksOf (run: GlyphRun) : uint32 array =
    [| for info in run.Infos -> info.Mask |]

/// <summary>Builds a font that has a cmap and horizontal metrics, the advance of glyph <c>n</c> being the <c>n</c>-th of
/// the given advances.</summary>
/// <param name="mapping">The code point and glyph pairs.</param>
/// <param name="advances">The advance of each glyph, from glyph 0.</param>
/// <returns>The font.</returns>
let fontMappingWithAdvances (mapping: (uint32 * uint32) list) (advances: uint32 list) : Font =
    let sorted = mapping |> List.sortBy fst
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 10u; be32 12u; format12 sorted ]
    let count = uint32 advances.Length
    let maxp = Array.concat [ be32 0x00005000u; be16 count ]
    let hhea = Array.concat [ be32 0x00010000u; be16 0x0200u; be16 0x0010u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0x0100u; Array.zeroCreate 16; be16 count ]
    let hmtx = advances |> List.map (fun advance -> Array.concat [ be16 advance; be16 0u ]) |> Array.concat
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap; Tags.Maxp, maxp; Tags.Hhea, hhea; Tags.Hmtx, hmtx ])) 0u |> Font.create

/// <summary>Shapes the given code points of a script through the whole pipeline, left to right, at the cluster level
/// monotone graphemes.</summary>
/// <param name="font">The font.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The shaped run.</returns>
let shaped (font: Font) (script: Script) (codepoints: uint32 array) : GlyphRun =
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
        |> GlyphRun.withSegment { Direction = Direction.Ltr; Script = script; Language = NoLanguage }
        |> GlyphRun.withClusterLevel ClusterLevel.MonotoneGraphemes
        |> TailCalls.settle
    match Shape.shape font ImmutableArray<Feature>.Empty run with
    | Ok result -> result
    | Error error -> failtest $"expected a shaped run, got {error}"

/// <summary>Builds a coverage table of the format with a glyph array.</summary>
/// <param name="glyphs">The glyphs, ascending.</param>
/// <returns>The table.</returns>
let private coverageTable (glyphs: uint32 list) : byte array =
    Array.concat [ be16 1u; be16 (uint32 glyphs.Length); glyphs |> List.map be16 |> Array.concat ]

/// <summary>Builds a single substitution lookup of the format that adds a delta to every glyph of a coverage.</summary>
/// <param name="glyphs">The glyphs it covers.</param>
/// <param name="delta">The delta added to each.</param>
/// <returns>The lookup table.</returns>
let singleLookup (glyphs: uint32 list) (delta: uint32) : byte array =
    let coverage = coverageTable (glyphs |> List.distinct |> List.sort)
    let subtable = Array.concat [ be16 1u; be16 6u; be16 (delta &&& 0xFFFFu); coverage ]
    Array.concat [ be16 1u; be16 0u; be16 1u; be16 8u; subtable ]

/// <summary>Builds a ligature substitution subtable of the format with one ligature of two glyphs for each first glyph and
/// second glyph.</summary>
/// <param name="firsts">The first glyphs.</param>
/// <param name="seconds">The second glyphs.</param>
/// <param name="ligature">The ligature glyph.</param>
/// <returns>The subtable.</returns>
let ligatureSubtable (firsts: uint32 list) (seconds: uint32 list) (ligature: uint32) : byte array =
    let firsts = firsts |> List.distinct |> List.sort
    let seconds = seconds |> List.distinct |> List.sort
    let joined (second: uint32) = Array.concat [ be16 ligature; be16 2u; be16 second ]
    let set =
        let header = 2u + 2u * uint32 seconds.Length
        Array.concat ([ be16 (uint32 seconds.Length) ] @ (seconds |> List.mapi (fun index _ -> be16 (header + 6u * uint32 index))) @ (seconds |> List.map joined))
    let header = 6u + 2u * uint32 firsts.Length
    let coverageAt = header + uint32 set.Length * uint32 firsts.Length
    Array.concat (
        [ be16 1u; be16 coverageAt; be16 (uint32 firsts.Length) ]
        @ (firsts |> List.mapi (fun index _ -> be16 (header + uint32 set.Length * uint32 index)))
        @ (firsts |> List.map (fun _ -> set))
        @ [ coverageTable firsts ])

/// <summary>Builds a ligature substitution lookup of the given subtables.</summary>
/// <param name="subtables">The subtables, in order.</param>
/// <returns>The lookup table.</returns>
let ligatureLookupOf (subtables: byte array list) : byte array =
    let count = uint32 subtables.Length
    let offsets = subtables |> List.scan (fun offset table -> offset + uint32 table.Length) (6u + 2u * count)
    Array.concat ([ be16 4u; be16 0u; be16 count ] @ (List.take subtables.Length offsets |> List.map be16) @ subtables)

/// <summary>Builds a ligature substitution lookup of one subtable with one ligature of two glyphs for each first glyph and
/// second glyph.</summary>
/// <param name="firsts">The first glyphs.</param>
/// <param name="seconds">The second glyphs.</param>
/// <param name="ligature">The ligature glyph.</param>
/// <returns>The lookup table.</returns>
let ligatureLookup (firsts: uint32 list) (seconds: uint32 list) (ligature: uint32) : byte array =
    ligatureLookupOf [ ligatureSubtable firsts seconds ligature ]

/// <summary>Builds a GSUB table with one script whose default language system lists every feature, and one lookup list.</summary>
/// <param name="script">The script tag.</param>
/// <param name="features">The features, each with the lookups it names.</param>
/// <param name="lookups">The lookup tables, in order.</param>
/// <returns>The table.</returns>
let gsubOf (script: string) (features: (string * uint32 list) list) (lookups: byte array list) : byte array =
    let sorted = features |> List.sortBy fst
    let ascii (text: string) = Text.Encoding.ASCII.GetBytes text
    let count = uint32 sorted.Length
    let langSys = Array.concat [ be16 0u; be16 0xFFFFu; be16 count; [ 0u .. count - 1u ] |> List.map be16 |> Array.concat ]
    let scriptTable = Array.concat [ be16 4u; be16 0u; langSys ]
    let scriptList = Array.concat [ be16 1u; ascii script; be16 8u; scriptTable ]
    let featureTables = sorted |> List.map (fun (_, indices) -> Array.concat [ be16 0u; be16 (uint32 indices.Length); indices |> List.map be16 |> Array.concat ])
    let featureOffsets = featureTables |> List.scan (fun offset table -> offset + uint32 table.Length) (2u + 6u * count)
    let featureList =
        Array.concat (
            [ be16 count ]
            @ (List.map2 (fun (tag, _) offset -> Array.concat [ ascii tag; be16 offset ]) sorted (List.take sorted.Length featureOffsets))
            @ featureTables)
    let lookupCount = uint32 lookups.Length
    let lookupOffsets = lookups |> List.scan (fun offset table -> offset + uint32 table.Length) (2u + 2u * lookupCount)
    let lookupList = Array.concat ([ be16 lookupCount ] @ (List.take lookups.Length lookupOffsets |> List.map be16) @ lookups)
    let featureAt = 10u + uint32 scriptList.Length
    Array.concat [ be16 1u; be16 0u; be16 10u; be16 featureAt; be16 (featureAt + uint32 featureList.Length); scriptList; featureList; lookupList ]

/// <summary>Builds a font that has a cmap, a glyph count, horizontal metrics of 500 units for every glyph and a GSUB table.</summary>
/// <param name="mapping">The code point and glyph pairs.</param>
/// <param name="glyphCount">The number of glyphs.</param>
/// <param name="gsub">The GSUB table.</param>
/// <returns>The font.</returns>
let fontWithGsub (mapping: (uint32 * uint32) list) (glyphCount: uint32) (gsub: byte array) : Font =
    let sorted = mapping |> List.sortBy fst
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 10u; be32 12u; format12 sorted ]
    let maxp = Array.concat [ be32 0x00005000u; be16 glyphCount ]
    let hhea = Array.concat [ be32 0x00010000u; be16 0x0200u; be16 0x0010u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0x0100u; Array.zeroCreate 16; be16 glyphCount ]
    let hmtx = Array.create (int glyphCount) (Array.concat [ be16 500u; be16 0u ]) |> Array.concat
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap; Tags.Maxp, maxp; Tags.Hhea, hhea; Tags.Hmtx, hmtx; Tags.Gsub, gsub ])) 0u |> Font.create
