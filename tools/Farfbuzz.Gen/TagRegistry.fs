module Farfbuzz.Gen.TagRegistry

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

/// <summary>The OpenType language-system-tag registry after parsing.</summary>
type OtRegistry =
    { Header: string
      Names: Map<string, string>
      Ranks: Map<string, int>
      ToBcp47: Map<string, Set<string>>
      FromBcp47: Map<string, Set<string>> }

/// <summary>The BCP 47 language subtag registry after parsing.</summary>
type Bcp47Registry =
    { Header: string
      Names: Map<string, string>
      Scopes: Map<string, string>
      Macrolanguages: Map<string, Set<string>>
      Prefixes: Map<string, Set<string>>
      Grandfathered: Set<string> }

/// <summary>Reads the ISO 639-3 to ISO 639-1 dictionary literal from the upstream generator.</summary>
/// <param name="upstreamGenerator">The path to gen-tag-table.py.</param>
/// <returns>The three-letter to two-letter ISO language code mappings.</returns>
let iso639_3To1 (upstreamGenerator: string) : Map<string, string> =
    let lines = File.ReadAllLines upstreamGenerator
    let start = lines |> Array.findIndex (fun line -> line.Trim() = "ISO_639_3_TO_1 = {")
    let finish =
        lines
        |> Array.indexed
        |> Array.skip start
        |> Array.pick (fun (index, line) -> if line.Trim() = "}" then Some index else None)
    let pattern = Regex(@"^\s*'([^']+)'\s*:\s*'([^']+)',?\s*$")
    lines
    |> Array.skip (start + 1)
    |> Array.take (finish - start - 1)
    |> Array.choose (fun line ->
        let matched = pattern.Match(line)
        match matched.Success with
        | true -> Some(matched.Groups[1].Value, matched.Groups[2].Value)
        | false -> None)
    |> Map.ofArray

/// <summary>Parses the OpenType language-system-tag registry page.</summary>
/// <param name="html">The page HTML.</param>
/// <param name="iso">The ISO 639-3 to ISO 639-1 mapping.</param>
/// <returns>The registry, or an error.</returns>
let parseOt (html: string) (iso: Map<string, string>) : Result<OtRegistry, string> =
    let struct (header, rows) = HtmlRows.parse html
    match header with
    | ValueNone -> Error "OpenType registry has no updated_at meta tag"
    | ValueSome headerText ->
        let names = Dictionary<string, string>()
        let ranks = Dictionary<string, int>()
        let toBcp47 = Dictionary<string, Set<string>>()
        let errors = ResizeArray<string>()
        for row in rows do
            match row.Length with
            | length when length < 2 || length > 3 -> errors.Add $"OpenType row has {length} cells"
            | _ ->
                let mutable name = row[0].Trim()
                let mutable tag = row[1].Trim([| '\t'; '\n'; '\v'; '\f'; '\r'; ' '; '\'' |])
                let mutable rank = 0
                match tag.Length > 4 with
                | true ->
                    match tag.EndsWith " (deprecated)" with
                    | true ->
                        name <- name + " (deprecated)"
                        tag <- tag.Split(' ')[0]
                        rank <- 1
                    | false -> errors.Add $"ill-formed OpenType tag: {tag}"
                | false -> ()
                names[tag] <- if name.EndsWith " languages" then name.Substring(0, name.Length - 10) else name
                let isoCell = if row.Length = 3 then row[2] else ""
                match isoCell.Length with
                | 0 -> ()
                | _ ->
                    let mapped =
                        isoCell.Trim().Replace(" ", "").Split(',')
                        |> Array.map (fun code -> match iso.TryGetValue code with | true, value -> value | false, _ -> code)
                        |> Set.ofArray
                    let combined =
                        match toBcp47.TryGetValue tag with
                        | true, existing -> Set.union existing mapped
                        | false, _ -> mapped
                    toBcp47[tag] <- combined
                    ranks[tag] <- rank + 2 * combined.Count
        match errors.Count with
        | count when count > 0 -> Error(String.Join("\n", errors))
        | _ ->
            let fromBcp47 = Dictionary<string, Set<string>>()
            for pair in toBcp47 do
                for isoCode in pair.Value do
                    let existing = match fromBcp47.TryGetValue isoCode with | true, set -> set | false, _ -> Set.empty
                    fromBcp47[isoCode] <- existing.Add pair.Key
            Ok
                { Header = headerText
                  Names = names |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
                  Ranks = ranks |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
                  ToBcp47 = toBcp47 |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
                  FromBcp47 = fromBcp47 |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq }

/// <summary>Adds language to the group that already holds macrolanguage, if one does.</summary>
/// <param name="macrolanguages">The macrolanguage groups.</param>
/// <param name="macrolanguage">The macrolanguage to attach to.</param>
/// <param name="language">The language to attach.</param>
/// <returns>True when an existing group absorbed the language.</returns>
let private tryAttachToExistingGroup
    (macrolanguages: Dictionary<string, Set<string>>)
    (macrolanguage: string)
    (language: string) : bool =
    let keys = macrolanguages.Keys |> Seq.toArray
    let mutable attached = false
    let mutable i = 0
    while i < keys.Length && not attached do
        let set = macrolanguages[keys[i]]
        if set.Contains macrolanguage then
            macrolanguages[keys[i]] <- set.Add language
            attached <- true
        i <- i + 1
    attached

/// <summary>Adds one macrolanguage edge, mirroring BCP47Parser._add_macrolanguage.</summary>
/// <param name="macrolanguages">The macrolanguage groups.</param>
/// <param name="fromBcp47">The BCP 47 tags the OpenType registry maps.</param>
/// <param name="macrolanguage">The macrolanguage.</param>
/// <param name="language">The language inheriting from it.</param>
let rec private addMacrolanguage
    (macrolanguages: Dictionary<string, Set<string>>)
    (fromBcp47: Set<string>)
    (macrolanguage: string)
    (language: string) : unit =
    if not (fromBcp47.Contains language) then
        match macrolanguages.TryGetValue language with
        | true, languages ->
            for inherited in Set.toArray languages do
                addMacrolanguage macrolanguages fromBcp47 macrolanguage inherited
        | false, _ -> ()
    let attached =
        match fromBcp47.Contains macrolanguage with
        | true -> false
        | false -> tryAttachToExistingGroup macrolanguages macrolanguage language
    if not attached then
        let existing = match macrolanguages.TryGetValue macrolanguage with | true, set -> set | false, _ -> Set.empty
        macrolanguages[macrolanguage] <- existing.Add language

/// <summary>Splits registry text into its lines, dropping the final newline's empty line.</summary>
/// <param name="content">The registry text.</param>
/// <returns>The lines.</returns>
let private registryLines (content: string) : string list =
    use reader = new StringReader(content)
    let rec collect (acc: string list) =
        match reader.ReadLine() with
        | null -> List.rev acc
        | line -> collect (line :: acc)
    collect []

/// <summary>Parses the BCP 47 language subtag registry.</summary>
/// <param name="registry">The registry text.</param>
/// <param name="fromBcp47">The BCP 47 tags the OpenType registry maps.</param>
/// <returns>The registry, or an error.</returns>
let parseBcp47 (registry: string) (fromBcp47: Set<string>) : Result<Bcp47Registry, string> =
    let mutable subtagType = ""
    let mutable subtag = ""
    let mutable deprecated = false
    let mutable hasPreferredValue = false
    let mutable lineBuffer = ""
    let mutable header = ""
    let names = Dictionary<string, string>()
    let scopes = Dictionary<string, string>()
    let macrolanguages = Dictionary<string, Set<string>>()
    let prefixes = Dictionary<string, Set<string>>()
    let grandfathered = HashSet<string>()
    let lines = Seq.append (registryLines registry) (Seq.singleton "")
    for raw in lines do
        let line = raw.TrimEnd()
        match line.StartsWith ' ' with
        | true -> lineBuffer <- lineBuffer + line.Substring(1)
        | false ->
            let logical = lineBuffer
            lineBuffer <- line
            match logical with
            | _ when logical.StartsWith "Type: " ->
                subtagType <- logical.Split(' ')[1]
                deprecated <- false
                hasPreferredValue <- false
            | _ when logical.StartsWith "Subtag: " || logical.StartsWith "Tag: " ->
                subtag <- logical.Split(' ')[1]
                if subtagType = "grandfathered" then grandfathered.Add(subtag.ToLowerInvariant()) |> ignore
            | _ when logical.StartsWith "Description: " ->
                let mutable description = (logical.Split([| ' ' |], 2)[1]).Replace(" (individual language)", "")
                description <- Regex.Replace(description, @" (\(family\)|\((individual |macro)language\)|languages)$", "")
                match names.TryGetValue subtag with
                | true, existing -> names[subtag] <- existing + "\n" + description
                | false, _ -> names[subtag] <- description
            | _ when subtagType = "language" || subtagType = "grandfathered" ->
                match logical with
                | _ when logical.StartsWith "Scope: " ->
                    match logical.Split(' ')[1] with
                    | "macrolanguage" -> scopes[subtag] <- " [macrolanguage]"
                    | "collection" -> scopes[subtag] <- " [collection]"
                    | _ -> ()
                | _ when logical.StartsWith "Deprecated: " ->
                    scopes[subtag] <- " (retired code)" + (match scopes.TryGetValue subtag with | true, value -> value | false, _ -> "")
                    deprecated <- true
                | _ when deprecated && logical.StartsWith "Comments: see " ->
                    for language in logical.Replace(",", "").Split(' ') |> Array.skip 2 do
                        addMacrolanguage macrolanguages fromBcp47 subtag language
                | _ when logical.StartsWith "Preferred-Value: " ->
                    hasPreferredValue <- true
                    addMacrolanguage macrolanguages fromBcp47 (logical.Split(' ')[1]) subtag
                | _ when not hasPreferredValue && logical.StartsWith "Macrolanguage: " ->
                    addMacrolanguage macrolanguages fromBcp47 (logical.Split(' ')[1]) subtag
                | _ -> ()
            | _ when subtagType = "variant" ->
                match logical with
                | _ when logical.StartsWith "Deprecated: " ->
                    scopes[subtag] <- " (retired code)" + (match scopes.TryGetValue subtag with | true, value -> value | false, _ -> "")
                | _ when logical.StartsWith "Prefix: " ->
                    let prefix = logical.Split(' ')[1]
                    let existing = match prefixes.TryGetValue subtag with | true, set -> set | false, _ -> Set.empty
                    prefixes[subtag] <- existing.Add prefix
                | _ -> ()
            | _ when logical.StartsWith "File-Date: " -> header <- logical
            | _ -> ()
    match header.Length with
    | 0 -> Error "BCP 47 registry has no File-Date header"
    | _ ->
        Ok
            { Header = header
              Names = names |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
              Scopes = scopes |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
              Macrolanguages = macrolanguages |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
              Prefixes = prefixes |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
              Grandfathered = grandfathered |> Set.ofSeq }
