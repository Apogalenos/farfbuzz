module Farfbuzz.Gen.AatMappings

open System
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen

/// <summary>The upstream src directory.</summary>
let private sourceDirectory (root: string) = Path.Combine(root, "upstream", "harfbuzz", "src")

/// <summary>Extracts the feature type and selector enum values from hb-aat-layout.h.</summary>
/// <param name="header">The header text.</param>
/// <returns>The enum name to value map.</returns>
let private enumValues (header: string) : Map<string, int> =
    let pattern = Regex(@"\b(HB_AAT_LAYOUT_FEATURE_(?:TYPE|SELECTOR)_\w+)\s*=\s*(0x[0-9A-Fa-f]+|\d+)")
    pattern.Matches header
    |> Seq.fold
        (fun map (m: Match) ->
            let value =
                let text = m.Groups[2].Value
                match text.StartsWith "0x" with
                | true -> Convert.ToInt32(text.Substring 2, 16)
                | false -> Int32.Parse(text, Globalization.CultureInfo.InvariantCulture)
            Map.add m.Groups[1].Value value map)
        Map.empty

/// <summary>Resolves one mapping value: an enum name or a raw integer cast.</summary>
/// <param name="enums">The enum map.</param>
/// <param name="text">The value text.</param>
/// <returns>The value.</returns>
let private parseValue (enums: Map<string, int>) (text: string) : int =
    let text = text.Trim()
    match text.StartsWith "HB_AAT_LAYOUT_FEATURE_" with
    | true -> enums[text]
    | false ->
        let cast = Regex.Match(text, @"\(hb_aat_layout_feature_(?:type|selector)_t\)\s*(\d+)")
        match cast.Success with
        | true -> int cast.Groups[1].Value
        | false -> failwith $"unparsable feature-mapping value: {text}"

/// <summary>One feature mapping extracted from hb-aat-layout.cc.</summary>
type private Mapping =
    { Tag: string
      FeatureType: int
      SelectorToEnable: int
      SelectorToDisable: int }

/// <summary>Extracts the 73 (78 at 15.1.0) feature mappings in source order.</summary>
/// <param name="cc">The hb-aat-layout.cc text.</param>
/// <param name="enums">The enum map.</param>
/// <returns>The mappings.</returns>
let private extractMappings (cc: string) (enums: Map<string, int>) : Mapping list =
    let pattern =
        Regex(@"\{HB_TAG \('(.)','(.)','(.)','(.)'\),\s*([^,]+),\s*([^,]+),\s*([^,]+)\}")
    pattern.Matches cc
    |> Seq.map (fun (m: Match) ->
        { Tag = String.Concat [ m.Groups[1].Value; m.Groups[2].Value; m.Groups[3].Value; m.Groups[4].Value ]
          FeatureType = parseValue enums m.Groups[5].Value
          SelectorToEnable = parseValue enums m.Groups[6].Value
          SelectorToDisable = parseValue enums m.Groups[7].Value })
    |> Seq.toList

/// <summary>Builds the generated AatFeatureMappings.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    let header = File.ReadAllText (Path.Combine(sourceDirectory root, "hb-aat-layout.h"))
    let cc = File.ReadAllText (Path.Combine(sourceDirectory root, "hb-aat-layout.cc"))
    let enums = enumValues header
    let mappings = extractMappings cc enums
    let tagValue (text: string) =
        ((uint32 (byte text[0]) <<< 24) ||| (uint32 (byte text[1]) <<< 16) ||| (uint32 (byte text[2]) <<< 8) ||| uint32 (byte text[3]))
    let rows =
        mappings
        |> List.map (fun mapping ->
            $"            {{ OtFeatureTag = 0x{tagValue mapping.Tag:X8}u<tag>; AatFeatureType = {mapping.FeatureType}us; SelectorToEnable = {mapping.SelectorToEnable}us; SelectorToDisable = {mapping.SelectorToDisable}us }}")
        |> String.concat "\n"
    let tags = mappings |> List.map (fun mapping -> tagValue mapping.Tag)
    match tags = (tags |> List.sort) with
    | false -> failwith "feature_mappings is not sorted by tag"
    | true -> ()
    [ "[<Struct; NoComparison; NoEquality>]"
      "type internal FeatureMapping ="
      "    { OtFeatureTag: Tag"
      "      AatFeatureType: uint16"
      "      SelectorToEnable: uint16"
      "      SelectorToDisable: uint16 }"
      ""
      "module internal AatFeatureMappings ="
      ""
      "    /// <summary>The OpenType-to-AAT feature mappings, <c>feature_mappings</c> (src/hb-aat-layout.cc:106-186).</summary>"
      "    let mappings : ImmutableArray<FeatureMapping> ="
      "        Sealed.ofSpan (ReadOnlySpan [|"
      rows
      "        |])"
      ""
      "    /// <summary>Binary-searches the generated feature mappings by tag.</summary>"
      "    /// <param name=\"tag\">The OpenType feature tag.</param>"
      "    /// <param name=\"lo\">The first candidate index.</param>"
      "    /// <param name=\"hi\">The last candidate index.</param>"
      "    /// <returns>The mapping, or none when the tag is absent.</returns>"
      "    [<TailCall>]"
      "    let rec private search (tag: Tag) (lo: int) (hi: int) : FeatureMapping voption ="
      "        match lo > hi with"
      "        | true -> ValueNone"
      "        | false ->"
      "            let mid = (lo + hi) / 2"
      "            let mapping = mappings[mid]"
      "            match uint32 tag < uint32 mapping.OtFeatureTag, uint32 tag > uint32 mapping.OtFeatureTag with"
      "            | true, _ -> search tag lo (mid - 1)"
      "            | false, true -> search tag (mid + 1) hi"
      "            | false, false -> ValueSome mapping"
      ""
      "    /// <summary>Finds the AAT feature and selectors of an OpenType tag, <c>hb_aat_layout_find_feature_mapping</c>.</summary>"
      "    /// <param name=\"tag\">The OpenType feature tag.</param>"
      "    /// <returns>The mapping, or none when the tag is absent.</returns>"
      "    let find (tag: Tag) : FeatureMapping voption ="
      "        search tag 0 (mappings.Length - 1)" ]
    |> String.concat "\n"
