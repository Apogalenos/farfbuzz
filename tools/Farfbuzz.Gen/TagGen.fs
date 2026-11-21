module Farfbuzz.Gen.TagGen

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen
open Farfbuzz.Gen.TagRegistry

/// <summary>The shape of one complex-language rule head, before it is emitted.</summary>
type Head =
    | UndOnly
    | Exact of string
    | Prefix of string
    | LangMatches of string

/// <summary>Everything the tag-table generator computes, in emission order.</summary>
type OtTagData =
    { Languages2: (string * string) list
      Languages3Blocked: string list
      Languages3: (string * string) list
      Languages3MultiValues: string list
      Languages3Multi: (string * int * int) list
      UndMinSubtagLength: int
      ComplexRules: (char * Head * string list * string list) list
      Ambiguous: (string * string) list }

/// <summary>A BCP 47 language tag split into its subtags, <c>LanguageTag</c>.</summary>
type private LanguageTag =
    { Subtags: string list
      Grandfathered: bool
      Language: string
      Script: string voption
      Region: string voption
      Variant: string voption }

/// <summary>The mutable OpenType registry state the overrides mutate.</summary>
type private OtState =
    { Names: Dictionary<string, string>
      Ranks: Dictionary<string, int>
      ToBcp47: Dictionary<string, Set<string>>
      FromBcp47: Dictionary<string, Set<string>>
      mutable FromBcp47Uninherited: Set<string> voption }

/// <summary>The mutable BCP 47 registry state the overrides mutate.</summary>
type private BcpState =
    { Names: Dictionary<string, string>
      Scopes: Dictionary<string, string>
      Macrolanguages: Dictionary<string, Set<string>>
      Prefixes: Dictionary<string, Set<string>>
      Grandfathered: Set<string> }

/// <summary>Copies a map into a mutable dictionary.</summary>
/// <param name="source">The map.</param>
/// <returns>The dictionary.</returns>
let private dictOf (source: Map<string, 'V>) =
    let target = Dictionary<string, 'V>()
    for pair in source do
        target[pair.Key] <- pair.Value
    target

/// <summary>Converts the parsed OpenType registry into its mutable state.</summary>
/// <param name="ot">The parsed registry.</param>
/// <returns>The mutable state.</returns>
let private toOtState (ot: OtRegistry) =
    { Names = dictOf ot.Names
      Ranks = dictOf ot.Ranks
      ToBcp47 = dictOf ot.ToBcp47
      FromBcp47 = dictOf ot.FromBcp47
      FromBcp47Uninherited = ValueNone }

/// <summary>Converts the parsed BCP 47 registry into its mutable state.</summary>
/// <param name="bcp">The parsed registry.</param>
/// <returns>The mutable state.</returns>
let private toBcpState (bcp: Bcp47Registry) =
    { Names = dictOf bcp.Names
      Scopes = dictOf bcp.Scopes
      Macrolanguages = dictOf bcp.Macrolanguages
      Prefixes = dictOf bcp.Prefixes
      Grandfathered = bcp.Grandfathered }

/// <summary>Packs a 1-4 character tag into 32 bits, padding with spaces, as <c>hb_tag</c>.</summary>
/// <param name="text">The tag.</param>
/// <returns>The packed value.</returns>
let private tagValue (text: string) : uint32 =
    match text with
    | "" -> 0u
    | _ ->
        let padded = (text + "    ").Substring(0, 4)
        (uint32 (int padded[0]) <<< 24) ||| (uint32 (int padded[1]) <<< 16) ||| (uint32 (int padded[2]) <<< 8) ||| uint32 (int padded[3])

/// <summary>Reads a rank, defaulting to zero like a <c>defaultdict(int)</c>.</summary>
/// <param name="ot">The OpenType state.</param>
/// <param name="tag">The tag.</param>
/// <returns>The rank, or zero.</returns>
let private rankOf (ot: OtState) (tag: string) : int =
    match ot.Ranks.TryGetValue tag with
    | true, value -> value
    | false, _ -> 0

/// <summary>Adds a language mapping, mirroring <c>OpenTypeRegistryParser.add_language</c>.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="ot">The OpenType state.</param>
/// <param name="bcp47Tag">The BCP 47 tag.</param>
/// <param name="otTag">The OpenType tag.</param>
let private addLanguage (bcp: BcpState) (ot: OtState) (bcp47Tag: string) (otTag: string) : unit =
    match ot.ToBcp47.TryGetValue otTag with
    | true, existing -> ot.ToBcp47[otTag] <- existing.Add bcp47Tag
    | false, _ -> ot.ToBcp47[otTag] <- Set.singleton bcp47Tag
    match ot.FromBcp47.TryGetValue bcp47Tag with
    | true, existing -> ot.FromBcp47[bcp47Tag] <- existing.Add otTag
    | false, _ -> ot.FromBcp47[bcp47Tag] <- Set.singleton otTag
    match bcp.Grandfathered.Contains (bcp47Tag.ToLowerInvariant()) with
    | true -> ()
    | false ->
        match bcp47Tag.IndexOf '-' with
        | -1 -> ()
        | dash ->
            let macrolanguage = bcp47Tag.Substring(0, dash)
            let suffix = bcp47Tag.Substring(dash + 1)
            match bcp.Macrolanguages.TryGetValue macrolanguage with
            | true, languages ->
                let generated =
                    languages
                    |> Set.filter (fun language -> not (bcp.Grandfathered.Contains (language.ToLowerInvariant())))
                    |> Set.map (fun language -> language + "-" + suffix)
                bcp.Macrolanguages[macrolanguage + "-" + suffix] <- generated
            | false, _ -> ()

/// <summary>Removes an OpenType tag, mirroring <c>remove_language_ot</c>.</summary>
/// <param name="ot">The OpenType state.</param>
/// <param name="otTag">The tag to remove.</param>
let private removeLanguageOt (ot: OtState) (otTag: string) : unit =
    let bcp47Tags = match ot.ToBcp47.TryGetValue otTag with | true, existing -> existing | false, _ -> Set.empty
    ot.ToBcp47.Remove otTag |> ignore
    for bcp47Tag in bcp47Tags do
        match ot.FromBcp47.TryGetValue bcp47Tag with
        | true, existing ->
            let remaining = existing.Remove otTag
            match Set.isEmpty remaining with
            | true -> ot.FromBcp47.Remove bcp47Tag |> ignore
            | false -> ot.FromBcp47[bcp47Tag] <- remaining
        | false, _ -> ()

/// <summary>Whether a subtag scope records it as retired.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="subtag">The subtag.</param>
/// <returns>True when the scope holds <c>retired code</c>.</returns>
let private scopeHasRetired (bcp: BcpState) (subtag: string) : bool =
    match bcp.Scopes.TryGetValue subtag with
    | true, scope -> scope.Contains "retired code"
    | false, _ -> false

/// <summary>Attaches a language to the group that already holds its macrolanguage.</summary>
/// <param name="macrolanguages">The groups.</param>
/// <param name="macrolanguage">The macrolanguage to attach to.</param>
/// <param name="language">The language to attach.</param>
/// <returns>True when an existing group absorbed the language.</returns>
let private tryAttachToExistingGroup (macrolanguages: Dictionary<string, Set<string>>) (macrolanguage: string) (language: string) : bool =
    let keys = macrolanguages.Keys |> Seq.toArray
    let mutable attached = false
    let mutable index = 0
    while index < keys.Length && not attached do
        let group = macrolanguages[keys[index]]
        if group.Contains macrolanguage then
            macrolanguages[keys[index]] <- group.Add language
            attached <- true
        index <- index + 1
    attached

/// <summary>Adds one macrolanguage edge, mirroring <c>BCP47Parser._add_macrolanguage</c>.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="fromBcp47Keys">The current OpenType-mapped BCP 47 keys.</param>
/// <param name="macrolanguage">The macrolanguage.</param>
/// <param name="language">The language inheriting from it.</param>
let rec private addMacrolanguage (bcp: BcpState) (fromBcp47Keys: Set<string>) (macrolanguage: string) (language: string) : unit =
    match fromBcp47Keys.Contains language with
    | false ->
        match bcp.Macrolanguages.TryGetValue language with
        | true, languages ->
            for inherited in Set.toArray languages do
                addMacrolanguage bcp fromBcp47Keys macrolanguage inherited
        | false, _ -> ()
    | true -> ()
    let attached =
        match fromBcp47Keys.Contains macrolanguage with
        | true -> false
        | false -> tryAttachToExistingGroup bcp.Macrolanguages macrolanguage language
    match attached with
    | true -> ()
    | false ->
        let existing = match bcp.Macrolanguages.TryGetValue macrolanguage with | true, set -> set | false, _ -> Set.empty
        bcp.Macrolanguages[macrolanguage] <- existing.Add language

/// <summary>Makes every language have at most one macrolanguage, <c>remove_extra_macrolanguages</c>.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="ot">The OpenType state.</param>
let private removeExtraMacrolanguages (bcp: BcpState) (ot: OtState) : unit =
    let inverted = Dictionary<string, ResizeArray<string>>()
    for pair in bcp.Macrolanguages do
        for language in pair.Value do
            match inverted.TryGetValue language with
            | true, existing -> existing.Add pair.Key
            | false, _ ->
                let list = ResizeArray<string>()
                list.Add pair.Key
                inverted[language] <- list
    let fromBcp47Keys = ot.FromBcp47.Keys |> Set.ofSeq
    for pair in inverted do
        match pair.Value.Count > 1 with
        | false -> ()
        | true ->
            let ordered =
                pair.Value
                |> Seq.indexed
                |> Seq.sortBy (fun (index, macrolanguage) -> struct (bcp.Macrolanguages[macrolanguage].Count, index))
                |> Seq.map (fun (_, macrolanguage) -> macrolanguage)
                |> Seq.toList
            let biggest = List.last ordered
            for macrolanguage in List.take (ordered.Length - 1) ordered do
                addMacrolanguage bcp fromBcp47Keys biggest macrolanguage

/// <summary>Copies mappings from macrolanguages to individual languages, <c>inherit_from_macrolanguages</c>.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="ot">The OpenType state.</param>
let private inheritFromMacrolanguages (bcp: BcpState) (ot: OtState) : unit =
    let firstTime = ot.FromBcp47Uninherited.IsNone
    match firstTime with
    | true -> ot.FromBcp47Uninherited <- ValueSome (ot.FromBcp47.Keys |> Set.ofSeq)
    | false -> ()
    let snapshot = bcp.Macrolanguages |> Seq.map (fun pair -> pair.Key, pair.Value) |> Seq.toList
    let uninherited = ot.FromBcp47Uninherited.Value
    for macrolanguage, languages in snapshot do
        let otMacrolanguages = HashSet<string>()
        match uninherited.Contains macrolanguage with
        | true ->
            match ot.FromBcp47.TryGetValue macrolanguage with
            | true, existing ->
                for tag in existing do
                    otMacrolanguages.Add tag |> ignore
            | false, _ -> ()
        | false -> ()
        let blocked = HashSet<string>()
        match scopeHasRetired bcp macrolanguage with
        | true -> ()
        | false ->
            for otMacrolanguage in otMacrolanguages do
                let toBcp = match ot.ToBcp47.TryGetValue otMacrolanguage with | true, existing -> existing | false, _ -> Set.empty
                let roundTripMacrolanguages = toBcp |> Set.filter (fun language -> not (scopeHasRetired bcp language))
                let roundTripLanguages = languages |> Set.filter (fun language -> not (scopeHasRetired bcp language))
                let intersection = Set.intersect roundTripMacrolanguages roundTripLanguages
                match Set.isEmpty intersection, intersection = roundTripLanguages with
                | false, false -> blocked.Add otMacrolanguage |> ignore
                | _ -> ()
        match otMacrolanguages.Count > 0 with
        | true ->
            for otMacrolanguage in otMacrolanguages do
                match blocked.Contains otMacrolanguage with
                | true -> ()
                | false ->
                    for language in languages do
                        addLanguage bcp ot language otMacrolanguage
                        match blocked.Count with
                        | 0 -> ot.Ranks[otMacrolanguage] <- rankOf ot otMacrolanguage + 1
                        | _ -> ()
        | false ->
            match firstTime with
            | false -> ()
            | true ->
                let languageArray = Set.toArray languages
                let mutable stop = false
                let mutable index = 0
                while index < languageArray.Length && not stop do
                    let language = languageArray[index]
                    match uninherited.Contains language with
                    | true ->
                        match ot.FromBcp47.TryGetValue language with
                        | true, existing ->
                            for tag in existing do
                                otMacrolanguages.Add tag |> ignore
                        | false, _ -> ()
                    | false -> otMacrolanguages.Clear()
                    match otMacrolanguages.Count with
                    | 0 -> stop <- true
                    | _ -> ()
                    index <- index + 1
                for otMacrolanguage in otMacrolanguages do
                    addLanguage bcp ot macrolanguage otMacrolanguage

/// <summary>Sets the default language system and blocks tricky three-letter tags.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="ot">The OpenType state.</param>
let private defaultLanguageSystemPass (bcp: BcpState) (ot: OtState) : unit =
    ot.Names[""] <- ""
    ot.Ranks[""] <- (ot.Ranks.Values |> Seq.max) + 1
    let threeUpper = Regex("^[A-Z]{3}$")
    for pair in ot.Names do
        let trickyOtTag = pair.Key
        match threeUpper.IsMatch trickyOtTag with
        | false -> ()
        | true ->
            let possible = trickyOtTag.ToLowerInvariant()
            let empty =
                match ot.FromBcp47.TryGetValue possible with
                | true, existing -> Set.isEmpty existing
                | false, _ -> true
            match bcp.Names.ContainsKey possible && empty with
            | true ->
                addLanguage bcp ot possible ""
                bcp.Macrolanguages[possible] <- Set.empty
            | false -> ()

/// <summary>The delta applied to a rank when sorting, <c>rank_delta</c>.</summary>
/// <param name="bcp47">The BCP 47 tag.</param>
/// <param name="otTag">The OpenType tag.</param>
/// <returns>The delta.</returns>
let private rankDelta (bcp47: string) (otTag: string) : int =
    match (bcp47 = "ak" && otTag = "AKA") || (bcp47 = "tw" && otTag = "TWI") with
    | true -> -1
    | false -> 0

/// <summary>Sorts each language's OpenType tags by rank, <c>sort_languages</c>.</summary>
/// <param name="ot">The OpenType state.</param>
/// <returns>The language to sorted-tag mapping.</returns>
let private sortLanguages (ot: OtState) : Dictionary<string, string list> =
    let sorted = Dictionary<string, string list>()
    for pair in ot.FromBcp47 do
        let language = pair.Key
        let tags = pair.Value |> Set.toList |> List.sortBy (fun tag -> (rankOf ot tag + rankDelta language tag, tag))
        sorted[language] <- tags
    sorted

/// <summary>Finds the first item satisfying a predicate, <c>LanguageTag._find_first</c>.</summary>
/// <param name="predicate">The predicate.</param>
/// <param name="items">The items.</param>
/// <returns>The first match, or none.</returns>
let private findFirst (predicate: string -> bool) (items: string list) : string voption =
    match List.tryFind predicate items with
    | Some item -> ValueSome item
    | None -> ValueNone

/// <summary>Builds a language tag from a BCP 47 tag, <c>LanguageTag.__init__</c>.</summary>
/// <param name="tag">The BCP 47 tag.</param>
/// <param name="grandfathered">The grandfathered tags, lowercased.</param>
/// <returns>The language tag.</returns>
let private languageTag (tag: string) (grandfathered: Set<string>) : LanguageTag =
    let lower = tag.ToLowerInvariant()
    let subtags = lower.Split('-') |> Array.toList
    match grandfathered.Contains lower with
    | true -> { Subtags = subtags; Grandfathered = true; Language = lower; Script = ValueNone; Region = ValueNone; Variant = ValueNone }
    | false ->
        let script = findFirst (fun s -> s.Length = 4 && s[0] > '9') subtags
        let region = findFirst (fun s -> (s.Length = 2 && s[0] > '9') || (s.Length = 3 && s[0] <= '9')) (List.tail subtags)
        let variant = findFirst (fun s -> s.Length > 4 || (s.Length = 4 && s[0] <= '9')) subtags
        { Subtags = subtags; Grandfathered = false; Language = subtags[0]; Script = script; Region = region; Variant = variant }

/// <summary>Whether a tag is too complex for a single <c>LangTag</c>, <c>LanguageTag.is_complex</c>.</summary>
/// <param name="lt">The language tag.</param>
/// <param name="fromBcp47">The OpenType-mapped BCP 47 sets.</param>
/// <returns>True for a complex tag.</returns>
let private isComplex (lt: LanguageTag) (fromBcp47: Dictionary<string, Set<string>>) : bool =
    let tagSet (key: string) = match fromBcp47.TryGetValue key with | true, existing -> existing | false, _ -> Set.empty
    let simple =
        lt.Subtags.Length = 1
        || (lt.Grandfathered && lt.Subtags[1].Length <> 3 && tagSet lt.Subtags[0] = tagSet lt.Language)
    not simple

/// <summary>The group a tag belongs to, <c>LanguageTag.get_group</c>.</summary>
/// <param name="lt">The language tag.</param>
/// <param name="prefixes">The variant prefixes.</param>
/// <returns>The group key.</returns>
let private getGroup (lt: LanguageTag) (prefixes: Dictionary<string, Set<string>>) : string =
    let inPrefixes =
        match lt.Variant with
        | ValueSome variant -> (match prefixes.TryGetValue variant with | true, existing -> existing.Count = 1 | false, _ -> false)
        | ValueNone -> false
    match lt.Language = "und" || inPrefixes with
    | true -> "und"
    | false -> lt.Language[0].ToString()

/// <summary>The head and subtags of a switch-group rule.</summary>
/// <param name="lt">The language tag.</param>
/// <returns>The head and the remaining subtag matches.</returns>
let private headAndSubtags (lt: LanguageTag) : struct (Head * string list) =
    match lt.Grandfathered with
    | true -> struct (Exact (lt.Language.Substring(1)), [])
    | false ->
        let mutable stringLiteral = lt.Language.Substring(1) + "-"
        let mutable script = lt.Script
        let mutable region = lt.Region
        match script with
        | ValueSome value ->
            stringLiteral <- stringLiteral + value
            script <- ValueNone
            match region with
            | ValueSome value -> stringLiteral <- stringLiteral + "-" + value; region <- ValueNone
            | ValueNone -> ()
        | ValueNone -> ()
        let head = match stringLiteral.EndsWith "-" with | true -> Prefix stringLiteral | false -> LangMatches stringLiteral
        let subtags =
            [ match script with | ValueSome value -> yield "-" + value | ValueNone -> ()
              match region with | ValueSome value -> yield "-" + value | ValueNone -> ()
              match lt.Variant with | ValueSome value -> yield "-" + value | ValueNone -> () ]
        struct (head, subtags)

/// <summary>The head and subtags of an und-group rule.</summary>
/// <param name="lt">The language tag.</param>
/// <returns>The head and the subtag matches.</returns>
let private undHeadAndSubtags (lt: LanguageTag) : struct (Head * string list) =
    let subtags =
        [ match lt.Script with | ValueSome value -> yield "-" + value | ValueNone -> ()
          match lt.Region with | ValueSome value -> yield "-" + value | ValueNone -> ()
          match lt.Variant with | ValueSome value -> yield "-" + value | ValueNone -> () ]
    struct (UndOnly, subtags)

/// <summary>Whether a tag is in the uninherited mapping.</summary>
/// <param name="ot">The OpenType state.</param>
/// <param name="tag">The BCP 47 tag.</param>
/// <returns>True when present.</returns>
let private uninheritedContains (ot: OtState) (tag: string) : bool =
    ot.FromBcp47Uninherited.Value.Contains tag

/// <summary>Builds and normalizes the disambiguation map, <c>verify_disambiguation_dict</c>.</summary>
/// <param name="bcp">The BCP 47 state.</param>
/// <param name="ot">The OpenType state.</param>
/// <param name="sortedFromBcp47">The sorted language-to-tag mapping.</param>
/// <param name="iso">The ISO 639-3 to 639-1 mapping.</param>
/// <returns>The disambiguation map.</returns>
let private verifyDisambiguation (bcp: BcpState) (ot: OtState) (sortedFromBcp47: Dictionary<string, string list>) (iso: Map<string, string>) : Dictionary<string, string> =
    let disambiguation = Dictionary<string, string>()
    for otTag, bcp47Tag in
        [ ("ALT", "alt"); ("ARK", "rki"); ("ATH", "ath"); ("BHI", "bhb"); ("BLN", "bjt"); ("BTI", "beb")
          ("CCHN", "cco"); ("CMR", "swb"); ("CPP", "crp"); ("CRR", "crx"); ("DUJ", "dwu"); ("ECR", "crj")
          ("HAL", "cfm"); ("HND", "hnd"); ("HYE", "hyw"); ("KIS", "kqs"); ("KUI", "uki"); ("LRC", "bqi")
          ("NDB", "nd"); ("NIS", "njz"); ("PLG", "pce"); ("PRO", "pro"); ("QIN", "bgr"); ("QUH", "quh")
          ("QVI", "qvi"); ("QWH", "qwh"); ("SIG", "stv"); ("SRB", "sr"); ("SXT", "xnj"); ("ZHH", "zh-HK")
          ("ZHS", "zh-Hans"); ("ZHT", "zh-Hant"); ("ZHTM", "zh-MO") ] do
        disambiguation[otTag] <- bcp47Tag

    let fromBcp47 (tag: string) : string list = match sortedFromBcp47.TryGetValue tag with | true, existing -> existing | false, _ -> []
    let scopeOf (tag: string) : string = match bcp.Scopes.TryGetValue tag with | true, existing -> existing | false, _ -> ""
    let sameTag (language: string) (tags: string list) = language.Length = 3 && tags.Length = 1 && language = tags[0].ToLowerInvariant()

    for pair in ot.ToBcp47 do
        let otTag = pair.Key
        let bcp47Tags = pair.Value
        let primaryTags =
            match otTag with
            | "" -> []
            | _ ->
                bcp47Tags
                |> Set.filter (fun tag -> not (bcp.Grandfathered.Contains tag) && (fromBcp47 tag)[0] = otTag)
                |> Set.toList
        match primaryTags.Length with
        | 1 ->
            match disambiguation.ContainsKey otTag with
            | true -> failwith $"unnecessary disambiguation for OT tag: {otTag}"
            | false -> ()
            let primary = primaryTags[0]
            match primary.Contains "-" with
            | true -> disambiguation[otTag] <- primary
            | false ->
                let firstTag =
                    bcp47Tags
                    |> Set.filter (fun tag -> not (bcp.Grandfathered.Contains tag) && List.contains otTag (fromBcp47 tag))
                    |> Set.toList
                    |> List.sortBy (fun tag -> (tag.Length, tag))
                    |> List.head
                match primary <> firstTag with
                | true -> disambiguation[otTag] <- primary
                | false -> ()
        | 0 ->
            match disambiguation.ContainsKey otTag with
            | true -> failwith $"There is no possible valid disambiguation for {otTag}"
            | false -> ()
        | _ ->
            let originalLanguages =
                primaryTags
                |> List.filter (fun tag -> uninheritedContains ot tag && not ((scopeOf tag).Contains "retired code"))
            let mutable macrolanguages =
                match originalLanguages.Length with
                | 1 -> originalLanguages
                | _ -> primaryTags |> List.filter (fun tag -> scopeOf tag = " [macrolanguage]")
            match macrolanguages.Length with
            | n when n <> 1 -> macrolanguages <- primaryTags |> List.filter (fun tag -> scopeOf tag = " [collection]")
            | _ -> ()
            match macrolanguages.Length with
            | n when n <> 1 -> macrolanguages <- primaryTags |> List.filter (fun tag -> not ((scopeOf tag).Contains "retired code"))
            | _ -> ()
            match macrolanguages.Length with
            | n when n <> 1 ->
                let isoOf (code: string) = match iso.TryGetValue code with | true, value -> value | false, _ -> code
                macrolanguages <- primaryTags |> List.filter (fun tag -> tag.ToLowerInvariant() = isoOf (otTag.ToLowerInvariant()))
            | _ -> ()
            match macrolanguages.Length with
            | n when n <> 1 -> macrolanguages <- primaryTags |> List.filter (fun tag -> not (tag.Contains "-"))
            | _ -> ()
            match macrolanguages.Length with
            | n when n <> 1 ->
                let joined = String.Join(" ", List.sort primaryTags)
                match disambiguation.ContainsKey otTag with
                | false -> failwith $"ambiguous OT tag: {otTag} {joined}"
                | true ->
                    match bcp47Tags.Contains disambiguation[otTag] with
                    | false -> failwith $"{disambiguation[otTag]} is not a valid disambiguation for {otTag}"
                    | true -> ()
            | _ ->
                match disambiguation.ContainsKey otTag with
                | false -> disambiguation[otTag] <- macrolanguages[0]
                | true -> ()
            let value = disambiguation[otTag]
            match value.Contains "-" with
            | true -> ()
            | false ->
                let differentTags =
                    bcp47Tags
                    |> Set.filter (fun tag -> not (sameTag tag (fromBcp47 tag)))
                    |> Set.toList
                    |> List.sortBy (fun tag -> (tag.Length, tag))
                match differentTags with
                | first :: _ when value = first -> disambiguation.Remove otTag |> ignore
                | _ -> ()

    for pair in disambiguation do
        match ot.ToBcp47.ContainsKey pair.Key with
        | true -> ()
        | false -> failwith $"unknown OT tag: {pair.Key}"
    disambiguation

/// <summary>Runs the tag-table algorithm over the vendored inputs and emits the data.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The generated data.</returns>
let build (root: string) : OtTagData =
    let vendored = Path.Combine(root, "tools", "data", "vendored")
    let otHtml = File.ReadAllText(Path.Combine(vendored, "languagetags.html"))
    let bcpText = File.ReadAllText(Path.Combine(vendored, "language-subtag-registry"))
    let generator = Path.Combine(root, "upstream", "harfbuzz", "src", "gen-tag-table.py")
    let iso = TagRegistry.iso639_3To1 generator
    let otRegistry =
        match TagRegistry.parseOt otHtml iso with
        | Ok registry -> registry
        | Error error -> failwith error
    let fromBcpKeys = otRegistry.FromBcp47 |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    let bcpRegistry =
        match TagRegistry.parseBcp47 bcpText fromBcpKeys with
        | Ok registry -> registry
        | Error error -> failwith error
    let bcp = toBcpState bcpRegistry
    let ot = toOtState otRegistry

    addLanguage bcp ot "ary" "MOR"
    addLanguage bcp ot "ath" "ATH"
    addLanguage bcp ot "bai" "BML"
    ot.Ranks["BAL"] <- rankOf ot "KAR" + 1
    addLanguage bcp ot "ber" "BBR"
    removeLanguageOt ot "PGR"
    addLanguage bcp ot "el-polyton" "PGR"
    bcp.Names["flm"] <- "Falam Chin"
    bcp.Scopes["flm"] <- " (retired code)"
    bcp.Macrolanguages["flm"] <- Set.singleton "cfm"
    ot.Ranks["FNE"] <- rankOf ot "TNE" + 1
    addLanguage bcp ot "und-fonipa" "IPPH"
    addLanguage bcp ot "und-fonnapa" "APPH"
    addLanguage bcp ot "und-fonupa" "UPPH"
    addLanguage bcp ot "ga-Latg" "IRT"
    addLanguage bcp ot "hy-arevmda" "HYE"
    removeLanguageOt ot "KGE"
    addLanguage bcp ot "und-Geok" "KGE"
    addLanguage bcp ot "kht" "KHN"
    ot.Names["KHN"] <- ot.Names["KHT"] + " (Microsoft fonts)"
    ot.Ranks["KHN"] <- rankOf ot "KHT" + 1
    ot.Ranks["LCR"] <- rankOf ot "MCR" + 1
    ot.Names["MAL"] <- "Malayalam Traditional"
    ot.Ranks["MLR"] <- rankOf ot "MLR" + 1
    bcp.Names["mhv"] <- "Arakanese"
    bcp.Scopes["mhv"] <- " (retired code)"
    addLanguage bcp ot "mnw-TH" "MONT"
    addLanguage bcp ot "no" "NOR"
    addLanguage bcp ot "oc-provenc" "PRO"
    removeLanguageOt ot "QUZ"
    addLanguage bcp ot "qu" "QUZ"
    addLanguage bcp ot "qub" "QWH"
    addLanguage bcp ot "qud" "QVI"
    addLanguage bcp ot "qug" "QVI"
    addLanguage bcp ot "qul" "QUH"
    addLanguage bcp ot "qup" "QVI"
    addLanguage bcp ot "qur" "QWH"
    addLanguage bcp ot "qus" "QUH"
    addLanguage bcp ot "quw" "QVI"
    addLanguage bcp ot "qux" "QWH"
    addLanguage bcp ot "qva" "QWH"
    addLanguage bcp ot "qvh" "QWH"
    addLanguage bcp ot "qvj" "QVI"
    addLanguage bcp ot "qvl" "QWH"
    addLanguage bcp ot "qvm" "QWH"
    addLanguage bcp ot "qvn" "QWH"
    addLanguage bcp ot "qvo" "QVI"
    addLanguage bcp ot "qvp" "QWH"
    addLanguage bcp ot "qvw" "QWH"
    addLanguage bcp ot "qvz" "QVI"
    addLanguage bcp ot "qwa" "QWH"
    addLanguage bcp ot "qws" "QWH"
    addLanguage bcp ot "qxa" "QWH"
    addLanguage bcp ot "qxc" "QWH"
    addLanguage bcp ot "qxh" "QWH"
    addLanguage bcp ot "qxl" "QVI"
    addLanguage bcp ot "qxn" "QWH"
    addLanguage bcp ot "qxo" "QWH"
    addLanguage bcp ot "qxr" "QVI"
    addLanguage bcp ot "qxt" "QWH"
    addLanguage bcp ot "qxw" "QWH"
    match bcp.Macrolanguages.TryGetValue "ro-MD" with
    | true, existing -> bcp.Macrolanguages["ro-MD"] <- existing.Add "mo"
    | false, _ -> bcp.Macrolanguages["ro-MD"] <- Set.singleton "mo"
    removeLanguageOt ot "SYRE"
    removeLanguageOt ot "SYRJ"
    removeLanguageOt ot "SYRN"
    addLanguage bcp ot "und-Syre" "SYRE"
    addLanguage bcp ot "und-Syrj" "SYRJ"
    addLanguage bcp ot "und-Syrn" "SYRN"
    bcp.Names["xst"] <- "Silt'e"
    bcp.Scopes["xst"] <- " (retired code)"
    bcp.Macrolanguages["xst"] <- Set.ofList [ "stv"; "wle" ]
    addLanguage bcp ot "xwo" "TOD"
    removeLanguageOt ot "ZHH"
    removeLanguageOt ot "ZHP"
    removeLanguageOt ot "ZHT"
    removeLanguageOt ot "ZHTM"
    bcp.Macrolanguages["zh"] <- bcp.Macrolanguages["zh"].Remove "lzh"
    bcp.Macrolanguages["zh"] <- bcp.Macrolanguages["zh"].Remove "yue"
    addLanguage bcp ot "zh-Hant-MO" "ZHH"
    addLanguage bcp ot "zh-Hant-MO" "ZHTM"
    addLanguage bcp ot "zh-Hant-HK" "ZHH"
    addLanguage bcp ot "zh-Hans" "ZHS"
    addLanguage bcp ot "zh-Hant" "ZHT"
    addLanguage bcp ot "zh-HK" "ZHH"
    addLanguage bcp ot "zh-MO" "ZHH"
    addLanguage bcp ot "zh-MO" "ZHTM"
    addLanguage bcp ot "zh-TW" "ZHT"
    addLanguage bcp ot "lzh" "ZHT"
    addLanguage bcp ot "lzh-Hans" "ZHS"
    addLanguage bcp ot "yue" "ZHH"
    addLanguage bcp ot "yue-Hans" "ZHS"

    inheritFromMacrolanguages bcp ot
    removeExtraMacrolanguages bcp ot
    inheritFromMacrolanguages bcp ot
    defaultLanguageSystemPass bcp ot
    let sortedFromBcp47 = sortLanguages ot
    let sortedItems =
        sortedFromBcp47
        |> Seq.map (fun pair -> pair.Key, pair.Value)
        |> Seq.sortBy fst
        |> Seq.toList

    let languages2 =
        sortedItems
        |> List.filter (fun (language, _) -> language <> "" && not (language.Contains "-") && language.Length = 2)
        |> List.collect (fun (language, tags) -> tags |> List.map (fun tag -> (language, tag)))

    let sameTag (language: string) (tags: string list) = language.Length = 3 && tags.Length = 1 && language = tags[0].ToLowerInvariant()
    let languages3Blocked = ResizeArray<string>()
    let languages3 = ResizeArray<string * string>()
    let languages3MultiValues = ResizeArray<string>()
    let languages3Multi = ResizeArray<string * int * int>()
    for language, tags in sortedItems do
        match language <> "" && not (language.Contains "-") && language.Length = 3 with
        | false -> ()
        | true ->
            match sameTag language tags with
            | true -> ()
            | false ->
                match tags with
                | [ "" ] -> languages3Blocked.Add language
                | [ single ] -> languages3.Add (language, single)
                | _ ->
                    let offset = languages3MultiValues.Count
                    for tag in tags do
                        languages3MultiValues.Add tag
                    languages3Multi.Add (language, offset, tags.Length)

    let complexItems =
        sortedFromBcp47
        |> Seq.map (fun pair -> pair.Key, pair.Value)
        |> Seq.sortBy (fun (language, _) -> struct (-language.Length, language))
        |> Seq.map (fun (language, tags) -> (languageTag language bcp.Grandfathered, tags))
        |> Seq.filter (fun (lt, _) -> isComplex lt ot.FromBcp47)
        |> Seq.toList

    let groups = Dictionary<string, ResizeArray<LanguageTag * string list>>()
    for lt, tags in complexItems do
        let key = getGroup lt bcp.Prefixes
        match groups.TryGetValue key with
        | true, existing -> existing.Add (lt, tags)
        | false, _ ->
            let list = ResizeArray<LanguageTag * string list>()
            list.Add (lt, tags)
            groups[key] <- list

    let undItems =
        match groups.TryGetValue "und" with
        | true, existing -> List.ofSeq existing
        | false, _ -> []
    let undItemsNonEmpty = undItems |> List.filter (fun (_, tags) -> not (List.isEmpty tags))
    let undMinSubtagLength =
        undItemsNonEmpty
        |> List.fold
            (fun acc (lt, _) ->
                let length =
                    (match lt.Script with | ValueSome value -> 1 + value.Length | ValueNone -> 0)
                    + (match lt.Region with | ValueSome value -> 1 + value.Length | ValueNone -> 0)
                    + (match lt.Variant with | ValueSome value -> 1 + value.Length | ValueNone -> 0)
                min acc length)
            100
    let undRules =
        undItemsNonEmpty
        |> List.map (fun (lt, tags) ->
            let struct (head, subtags) = undHeadAndSubtags lt
            (lt.Language[0], head, subtags, tags))
    let switchRules =
        groups.Keys
        |> Seq.filter (fun key -> key <> "und")
        |> Seq.sort
        |> Seq.collect (fun key ->
            groups[key]
            |> Seq.filter (fun (_, tags) -> not (List.isEmpty tags))
            |> Seq.map (fun (lt, tags) ->
                let struct (head, subtags) = headAndSubtags lt
                (lt.Language[0], head, subtags, tags)))
        |> Seq.toList

    let disambiguation = verifyDisambiguation bcp ot sortedFromBcp47 iso
    let ambiguous = disambiguation |> Seq.map (fun pair -> pair.Key, pair.Value) |> Seq.sortBy fst |> Seq.toList

    { Languages2 = languages2
      Languages3Blocked = List.ofSeq languages3Blocked
      Languages3 = List.ofSeq languages3
      Languages3MultiValues = List.ofSeq languages3MultiValues
      Languages3Multi = List.ofSeq languages3Multi
      UndMinSubtagLength = undMinSubtagLength
      ComplexRules = undRules @ switchRules
      Ambiguous = ambiguous }

/// <summary>Escapes a string for an F# literal.</summary>
/// <param name="text">The text.</param>
/// <returns>The escaped text.</returns>
let private escapeText (text: string) =
    text.Replace("\\", "\\\\").Replace("\"", "\\\"")

/// <summary>Formats a tag as an F# tag literal.</summary>
/// <param name="tag">The tag string.</param>
/// <returns>The literal.</returns>
let private hexTag (tag: string) = $"0x{tagValue tag:X8}u<tag>"

/// <summary>Formats a string as an F# string literal.</summary>
/// <param name="text">The text.</param>
/// <returns>The literal.</returns>
let private stringLit (text: string) = $"\"{escapeText text}\""

/// <summary>Formats the head of a complex rule.</summary>
/// <param name="head">The head.</param>
/// <returns>The F# expression.</returns>
let private headText (head: Head) =
    match head with
    | UndOnly -> "UndOnly"
    | Exact value -> $"Exact {stringLit value}"
    | Prefix value -> $"Prefix {stringLit value}"
    | LangMatches value -> $"LangMatches {stringLit value}"

/// <summary>Formats an immutable string array expression.</summary>
/// <param name="values">The strings.</param>
/// <returns>The expression.</returns>
let private stringArray (values: string list) =
    match values with
    | [] -> "Sealed.ofSpan (ReadOnlySpan [||])"
    | _ ->
        let items = values |> List.map stringLit |> String.concat "; "
        $"Sealed.ofSpan (ReadOnlySpan [| {items} |])"

/// <summary>Formats an immutable tag array expression.</summary>
/// <param name="values">The tag strings.</param>
/// <returns>The expression.</returns>
let private tagArray (values: string list) =
    match values with
    | [] -> "Sealed.ofSpan (ReadOnlySpan [||])"
    | _ ->
        let items = values |> List.map hexTag |> String.concat "; "
        $"Sealed.ofSpan (ReadOnlySpan [| {items} |])"

/// <summary>Formats one LangTag record.</summary>
/// <param name="language">The language tag.</param>
/// <param name="otTag">The OpenType tag.</param>
/// <returns>The record expression.</returns>
let private langTagRecord (language: string) (otTag: string) =
    $"{{ Language = {hexTag language}; OtTag = {hexTag otTag} }}"

/// <summary>Formats one LangTagRange record.</summary>
/// <param name="language">The language tag.</param>
/// <param name="offset">The offset.</param>
/// <param name="count">The count.</param>
/// <returns>The record expression.</returns>
let private langTagRangeRecord (language: string) (offset: int) (count: int) =
    $"{{ RangeLanguage = {hexTag language}; Offset = {offset}us; Count = {count}uy }}"

/// <summary>Formats one ComplexRule record.</summary>
/// <param name="group">The group.</param>
/// <param name="head">The head.</param>
/// <param name="subtags">The subtags.</param>
/// <param name="tags">The tags.</param>
/// <returns>The record expression.</returns>
let private complexRuleRecord (group: char) (head: Head) (subtags: string list) (tags: string list) =
    $"{{ Group = '{group}'; Head = {headText head}; Subtags = {stringArray subtags}; Tags = {tagArray tags} }}"

/// <summary>Formats an immutable array binding.</summary>
/// <param name="name">The binding name.</param>
/// <param name="typeName">The element type.</param>
/// <param name="elements">The element expressions.</param>
/// <returns>The binding text.</returns>
let private arrayBinding (name: string) (typeName: string) (elements: string list) =
    match elements with
    | [] -> $"let {name} : {typeName} =\n    Sealed.ofSpan (ReadOnlySpan [||])"
    | _ ->
        let rows = elements |> List.map (fun element -> "        " + element)
        $"let {name} : {typeName} =\n    Sealed.ofSpan (ReadOnlySpan [|\n" + String.concat ";\n" rows + "\n    |])"

/// <summary>Emits the generated tag-table body.</summary>
/// <param name="data">The data.</param>
/// <returns>The body text.</returns>
let emit (data: OtTagData) : string =
    let types =
        "[<Struct>]\ntype internal LangTag = { Language: Tag; OtTag: Tag }\n\n" +
        "[<Struct>]\ntype internal LangTagRange = { RangeLanguage: Tag; Offset: uint16; Count: byte }\n\n" +
        "[<Struct>]\ntype internal RuleHead =\n    | UndOnly\n    | Exact of exact: string\n    | Prefix of prefix: string\n    | LangMatches of spec: string\n\n" +
        "type internal ComplexRule = { Group: char; Head: RuleHead; Subtags: ImmutableArray<string>; Tags: ImmutableArray<Tag> }"
    let languages2 = data.Languages2 |> List.map (fun (language, tag) -> langTagRecord language tag)
    let languages3Blocked = data.Languages3Blocked |> List.map hexTag
    let languages3 = data.Languages3 |> List.map (fun (language, tag) -> langTagRecord language tag)
    let languages3MultiValues = data.Languages3MultiValues |> List.map hexTag
    let languages3Multi = data.Languages3Multi |> List.map (fun (language, offset, count) -> langTagRangeRecord language offset count)
    let complexRules =
        data.ComplexRules
        |> List.map (fun (group, head, subtags, tags) -> complexRuleRecord group head subtags tags)
    let ambiguous =
        data.Ambiguous
        |> List.map (fun (tag, bcp47) -> $"struct ({hexTag tag}, {stringLit bcp47})")
    let body =
        String.concat
            "\n\n"
            [ arrayBinding "languages2" "ImmutableArray<LangTag>" languages2
              arrayBinding "languages3Blocked" "ImmutableArray<Tag>" languages3Blocked
              arrayBinding "languages3" "ImmutableArray<LangTag>" languages3
              arrayBinding "languages3MultiValues" "ImmutableArray<Tag>" languages3MultiValues
              arrayBinding "languages3Multi" "ImmutableArray<LangTagRange>" languages3Multi
              $"let undMinSubtagLength = {data.UndMinSubtagLength}"
              arrayBinding "complexRules" "ImmutableArray<ComplexRule>" complexRules
              arrayBinding "ambiguous" "ImmutableArray<struct (Tag * string)>" ambiguous ]
    types + "\n\n" + Emit.moduleBlock "OtTagTable" body

/// <summary>Parses every <c>HB_TAG('a','b','c','d')</c> value in a section, in order.</summary>
/// <param name="text">The section text.</param>
/// <returns>The packed values.</returns>
let private parseHbTags (text: string) : uint32 list =
    Regex.Matches(text, @"HB_TAG\('(.)','(.)','(.)','(.)'\)")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> tagValue (m.Groups[1].Value + m.Groups[2].Value + m.Groups[3].Value + m.Groups[4].Value))
    |> Seq.toList

/// <summary>Extracts the text between two markers.</summary>
/// <param name="text">The full text.</param>
/// <param name="startMarker">The start marker.</param>
/// <param name="endMarker">The end marker.</param>
/// <returns>The section.</returns>
let private section (text: string) (startMarker: string) (endMarker: string) : string =
    let start = text.IndexOf startMarker
    match start with
    | -1 -> ""
    | start ->
        let finish = text.IndexOf(endMarker, start + startMarker.Length)
        match finish with
        | -1 -> text.Substring(start + startMarker.Length)
        | finish -> text.Substring(start + startMarker.Length, finish - (start + startMarker.Length))

/// <summary>Finds the matching close delimiter from an open delimiter.</summary>
/// <param name="text">The text.</param>
/// <param name="openIndex">The open delimiter index.</param>
/// <param name="openChar">The open character.</param>
/// <param name="closeChar">The close character.</param>
/// <returns>The close delimiter index, or -1.</returns>
let private findMatching (text: string) (openIndex: int) (openChar: char) (closeChar: char) : int =
    let mutable depth = 0
    let mutable index = openIndex
    let mutable result = -1
    while index < text.Length && result < 0 do
        let current = text[index]
        match current = openChar, current = closeChar with
        | true, _ -> depth <- depth + 1
        | false, true ->
            depth <- depth - 1
            match depth with
            | 0 -> result <- index
            | _ -> ()
        | false, false -> ()
        index <- index + 1
    result

/// <summary>Parses <c>if (…) { … }</c> blocks into their start index, condition and body.</summary>
/// <param name="text">The text.</param>
/// <returns>The blocks in order.</returns>
let private parseIfBlocks (text: string) : (int * string * string) list =
    let blocks = ResizeArray<int * string * string>()
    let mutable index = 0
    while index < text.Length do
        let ifIndex = text.IndexOf("if (", index)
        match ifIndex with
        | -1 -> index <- text.Length
        | ifIndex ->
            let parenOpen = ifIndex + 3
            let parenClose = findMatching text parenOpen '(' ')'
            match parenClose with
            | -1 -> index <- text.Length
            | parenClose ->
                let braceOpen = text.IndexOf('{', parenClose + 1)
                match braceOpen with
                | -1 -> index <- text.Length
                | braceOpen ->
                    let braceClose = findMatching text braceOpen '{' '}'
                    match braceClose with
                    | -1 -> index <- text.Length
                    | braceClose ->
                        let condition = text.Substring(parenOpen + 1, parenClose - parenOpen - 1)
                        let body = text.Substring(braceOpen + 1, braceClose - braceOpen - 1)
                        blocks.Add (ifIndex, condition, body)
                        index <- braceClose + 1
    List.ofSeq blocks

/// <summary>The canonical spelling of a generated complex rule, for verification.</summary>
/// <param name="group">The group.</param>
/// <param name="head">The head.</param>
/// <param name="subtags">The subtags.</param>
/// <param name="tags">The tags.</param>
/// <param name="includeGroup">Whether to include the group character.</param>
/// <returns>The canonical string.</returns>
let private canonicalRule (group: char) (head: Head) (subtags: string list) (tags: string list) (includeGroup: bool) =
    let kind, argument =
        match head with
        | UndOnly -> "U", ""
        | Exact value -> "E", value
        | Prefix value -> "P", value
        | LangMatches value -> "L", value
    let prefix = match includeGroup with | true -> $"{group}|" | false -> ""
    let subs = String.concat "," subtags
    let ts = String.concat "," tags
    $"{prefix}{kind}|{argument}|{subs}|{ts}"

/// <summary>Spells a packed tag as its trimmed four-character form.</summary>
/// <param name="value">The packed tag.</param>
/// <returns>The tag string.</returns>
let private tagString (value: uint32) : string =
    let chars = [| char (byte (value >>> 24)); char (byte (value >>> 16)); char (byte (value >>> 8)); char (byte value) |]
    (new string(chars)).TrimEnd(' ')

/// <summary>The canonical spelling of a parsed upstream rule condition and body.</summary>
/// <param name="condition">The condition.</param>
/// <param name="body">The body.</param>
/// <param name="includeGroup">Whether a group was supplied separately.</param>
/// <param name="group">The group, when included.</param>
/// <returns>The canonical string.</returns>
let private canonicalUpstream (condition: string) (body: string) (includeGroup: bool) (group: char) =
    let pSubtags = Regex(@"subtag_matches \(p, limit, ""(-[a-z0-9]+)"", \d+\)")
    let langSubtags = Regex(@"subtag_matches \(lang_str, limit, ""(-[a-z0-9]+)"", \d+\)")
    let tags = parseHbTags body |> List.map tagString
    let kind, argument, subtags =
        match condition.Contains "subtag_matches (p, limit," with
        | true ->
            let subs = pSubtags.Matches condition |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups[1].Value) |> Seq.toList
            "U", "", subs
        | false ->
            match condition.Contains "strcmp" with
            | true ->
                let matched = Regex.Match(condition, @"strcmp \(&lang_str\[1\], ""([^""]+)""\)")
                "E", matched.Groups[1].Value, []
            | false ->
                match condition.Contains "strncmp" with
                | true ->
                    let matched = Regex.Match(condition, @"strncmp \(&lang_str\[1\], ""([^""]+)"", \d+\)")
                    let subs = langSubtags.Matches condition |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups[1].Value) |> Seq.toList
                    "P", matched.Groups[1].Value, subs
                | false ->
                    let matched = Regex.Match(condition, @"lang_matches \(&lang_str\[1\], limit, ""([^""]+)"", \d+\)")
                    let subs = langSubtags.Matches condition |> Seq.cast<Match> |> Seq.map (fun m -> m.Groups[1].Value) |> Seq.toList
                    "L", matched.Groups[1].Value, subs
    let prefix = match includeGroup with | true -> $"{group}|" | false -> ""
    let subs = String.concat "," subtags
    let ts = String.concat "," tags
    $"{prefix}{kind}|{argument}|{subs}|{ts}"

/// <summary>Compares two lists, reporting the first difference.</summary>
/// <param name="errors">The error sink.</param>
/// <param name="label">The list name.</param>
/// <param name="expected">The upstream list.</param>
/// <param name="actual">The generated list.</param>
let private checkEqual (errors: ResizeArray<string>) (label: string) (expected: 'T list) (actual: 'T list) : unit =
    match expected.Length <> actual.Length with
    | true -> errors.Add $"{label}: length differs ({expected.Length} upstream vs {actual.Length} generated)"
    | false ->
        let mutable index = 0
        let mutable differing = -1
        while index < expected.Length && differing < 0 do
            match expected[index] = actual[index] with
            | true -> index <- index + 1
            | false -> differing <- index
        match differing with
        | -1 -> ()
        | differing -> errors.Add $"{label}: differs at index {differing}: upstream {expected[differing]} vs generated {actual[differing]}"

/// <summary>Verifies the generated data against the committed upstream header, entry for entry.</summary>
/// <param name="root">The repository root.</param>
/// <param name="data">The generated data.</param>
/// <returns>The list of discrepancies, empty when the data matches.</returns>
let verifyUpstream (root: string) (data: OtTagData) : string list =
    let path = Path.Combine(root, "upstream", "harfbuzz", "src", "hb-ot-tag-table.hh")
    let text = File.ReadAllText path
    let errors = ResizeArray<string>()

    let languages2Text = section text "static const LangTag ot_languages2[] = {" "};"
    let expectedLanguages2 =
        parseHbTags languages2Text
        |> List.chunkBySize 2
        |> List.map (fun chunk -> match chunk with | [ a; b ] -> (a, b) | _ -> failwith "odd ot_languages2")
    let actualLanguages2 = data.Languages2 |> List.map (fun (language, tag) -> (tagValue language, tagValue tag))
    checkEqual errors "ot_languages2" expectedLanguages2 actualLanguages2

    let blockedText = section text "static const hb_tag_t ot_languages3_blocked[] = {" "};"
    checkEqual errors "ot_languages3_blocked" (parseHbTags blockedText) (data.Languages3Blocked |> List.map tagValue)

    let languages3Text = section text "static const LangTag ot_languages3[] = {" "};"
    let expectedLanguages3 =
        parseHbTags languages3Text
        |> List.chunkBySize 2
        |> List.map (fun chunk -> match chunk with | [ a; b ] -> (a, b) | _ -> failwith "odd ot_languages3")
    let actualLanguages3 = data.Languages3 |> List.map (fun (language, tag) -> (tagValue language, tagValue tag))
    checkEqual errors "ot_languages3" expectedLanguages3 actualLanguages3

    let multiValuesText = section text "static const hb_tag_t ot_languages3_multi_values[] = {" "};"
    checkEqual errors "ot_languages3_multi_values" (parseHbTags multiValuesText) (data.Languages3MultiValues |> List.map tagValue)

    let multiText = section text "static const LangTagRange ot_languages3_multi[] = {" "};"
    let rangeRegex = Regex(@"HB_TAG\('(.)','(.)','(.)','(.)'\),\s*(\d+),\s*(\d+)")
    let expectedMulti =
        rangeRegex.Matches multiText
        |> Seq.cast<Match>
        |> Seq.map (fun m ->
            (tagValue (m.Groups[1].Value + m.Groups[2].Value + m.Groups[3].Value + m.Groups[4].Value), int m.Groups[5].Value, int m.Groups[6].Value))
        |> Seq.toList
    let actualMulti = data.Languages3Multi |> List.map (fun (language, offset, count) -> (tagValue language, offset, count))
    checkEqual errors "ot_languages3_multi" expectedMulti actualMulti

    let complexBody = section text "hb_ot_tags_from_complex_language (const char" "hb_ot_ambiguous_tag_to_language"
    let nMatch = Regex.Match(complexBody, @"limit - lang_str >= (\d+)")
    let mMatch = Regex.Match(complexBody, @"limit - p < (\d+)")
    match int nMatch.Groups[1].Value = data.UndMinSubtagLength + 2, int mMatch.Groups[1].Value = data.UndMinSubtagLength with
    | true, true -> ()
    | _ -> errors.Add $"und min subtag length: upstream {mMatch.Groups[1].Value} vs generated {data.UndMinSubtagLength}"

    let undStart = complexBody.IndexOf "goto out;"
    let undEnd = complexBody.IndexOf("out:", undStart)
    let undSection = complexBody.Substring(undStart, undEnd - undStart)
    let undRules = parseIfBlocks undSection |> List.filter (fun (_, condition, _) -> condition.Contains "subtag_matches (p, limit,")
    let expectedUnd = undRules |> List.map (fun (_, condition, body) -> canonicalUpstream condition body false ' ')
    let actualUnd =
        data.ComplexRules
        |> List.filter (fun (_, head, _, _) -> match head with | UndOnly -> true | _ -> false)
        |> List.map (fun (group, head, subtags, tags) -> canonicalRule group head subtags tags false)
    checkEqual errors "complex und rules" expectedUnd actualUnd

    let switchStart = complexBody.IndexOf "switch (lang_str[0])"
    let switchSection = complexBody.Substring switchStart
    let caseRegex = Regex(@"case '([a-z])':")
    let casePositions =
        caseRegex.Matches switchSection
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (m.Index, m.Groups[1].Value[0]))
        |> Seq.toList
    let switchBlocks = parseIfBlocks switchSection
    let expectedSwitch =
        switchBlocks
        |> List.map (fun (blockIndex, condition, body) ->
            let group =
                casePositions
                |> List.filter (fun (position, _) -> position <= blockIndex)
                |> List.last
                |> snd
            canonicalUpstream condition body true group)
    let actualSwitch =
        data.ComplexRules
        |> List.filter (fun (_, head, _, _) -> match head with | UndOnly -> false | _ -> true)
        |> List.map (fun (group, head, subtags, tags) -> canonicalRule group head subtags tags true)
    checkEqual errors "complex switch rules" expectedSwitch actualSwitch

    let ambiguousBody = section text "hb_ot_ambiguous_tag_to_language" "#endif"
    let ambiguousRegex = Regex(@"case HB_TAG\('(.)','(.)','(.)','(.)'\):.*?return hb_language_from_string \(""([^""]+)"", -1\);", RegexOptions.Singleline)
    let expectedAmbiguous =
        ambiguousRegex.Matches ambiguousBody
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (tagValue (m.Groups[1].Value + m.Groups[2].Value + m.Groups[3].Value + m.Groups[4].Value), m.Groups[5].Value))
        |> Seq.toList
    let actualAmbiguous = data.Ambiguous |> List.map (fun (tag, bcp47) -> (tagValue tag, bcp47))
    checkEqual errors "ambiguous switch" expectedAmbiguous actualAmbiguous

    List.ofSeq errors
