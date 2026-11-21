module Farfbuzz.Gen.Program

open System
open System.IO
open Farfbuzz.Gen

/// <summary>Generates one source file and its text.</summary>
/// <param name="relative">The output path under <c>src/Farfbuzz/Generated</c>.</param>
/// <param name="sources">The upstream sources.</param>
/// <param name="body">The body.</param>
/// <returns>The file name and text.</returns>
let private output (relative: string) (sources: string list) (body: string) =
    relative, Emit.file sources body

/// <summary>Builds every generated source file.</summary>
/// <returns>The file names and their text.</returns>
let generate () =
    let upstream = UpstreamSources.upstream ()
    let postNames = UpstreamSources.hbStrList (Path.Combine(upstream, "src/hb-ot-post-macroman.hh"))
    let cffStrings = UpstreamSources.hbStrList (Path.Combine(upstream, "src/hb-ot-cff1-std-str.hh"))
    match postNames.Length, cffStrings.Length with
    | 258, 391 -> ()
    | _ -> failwith $"Unexpected name counts: post {postNames.Length}, CFF {cffStrings.Length}"
    let cff = Path.Combine(upstream, "src/hb-ot-cff1-table.cc")
    let cmap = Path.Combine(upstream, "src/hb-ot-cmap-table.hh")
    let arabicSimplified = Path.Combine(upstream, "src/ArabicPUASimplified.txt")
    let arabicTraditional = Path.Combine(upstream, "src/ArabicPUATraditional.txt")
    let macRoman = UpstreamSources.macRomanPairs cmap
    let simplified = UpstreamSources.arabicPua arabicSimplified
    let traditional = UpstreamSources.arabicPua arabicTraditional
    let asPairs (items: struct (uint16 * uint16) list) =
        items |> List.map (fun struct (first, second) -> struct (uint32 first, uint32 second))
    let standardEncodingToCode = UpstreamSources.cArrayNumbers cff "standard_encoding_to_code"
    let expertEncodingToCode = UpstreamSources.cArrayNumbers cff "expert_encoding_to_code"
    let expertCharsetToSid = UpstreamSources.cArrayNumbers cff "expert_charset_to_sid"
    let expertSubsetCharsetToSid = UpstreamSources.cArrayNumbers cff "expert_subset_charset_to_sid"
    let expertCharsetSidToGid = UpstreamSources.cArrayPairs cff "expert_charset_sid_to_gid"
    let expertSubsetCharsetSidToGid = UpstreamSources.cArrayPairs cff "expert_subset_charset_sid_to_gid"
    let standardEncodingToSid = UpstreamSources.cArrayNumbers cff "standard_encoding_to_sid"
    let assertCount name expected actual =
        match actual = expected with
        | true -> ()
        | false -> failwith $"Unexpected {name} count in {cff}: expected {expected}, got {actual}"
    assertCount "standard_encoding_to_code" 150 standardEncodingToCode.Length
    assertCount "expert_encoding_to_code" 379 expertEncodingToCode.Length
    assertCount "expert_charset_to_sid" 166 expertCharsetToSid.Length
    assertCount "expert_subset_charset_to_sid" 87 expertSubsetCharsetToSid.Length
    assertCount "expert_charset_sid_to_gid" 165 expertCharsetSidToGid.Length
    assertCount "expert_subset_charset_sid_to_gid" 86 expertSubsetCharsetSidToGid.Length
    assertCount "standard_encoding_to_sid" 256 standardEncodingToSid.Length
    assertCount "MacRoman mapping" 128 macRoman.Length
    assertCount "Arabic simplified PUA" 223 simplified.Length
    assertCount "Arabic traditional PUA" 268 traditional.Length
    let cffSources = [ "src/hb-ot-cff1-table.cc"; "src/hb-ot-cff1-std-str.hh" ]
    let otTagData = TagGen.build (UpstreamSources.root ())
    let machineOutputs =
        [ for name, moduleName in [ "indic", "IndicMachine"; "khmer", "KhmerMachine"; "myanmar", "MyanmarMachine"; "use", "UseMachine" ] do
              let machine = Path.Combine(upstream, $"src/hb-ot-shaper-{name}-machine.hh")
              match RagelFlat.parse machine name with
              | Error error -> failwith error
              | Ok flat -> yield output $"{moduleName}.fs" [ $"src/hb-ot-shaper-{name}-machine.hh" ] (RagelFlat.emit flat) ]
    [ output "PostMacRomanNames.fs" [ "src/hb-ot-post-macroman.hh" ] (
          Emit.moduleBlock "PostMacRomanNames" (Emit.byteArrays "names" postNames))
      output "CffStandardStrings.fs" [ "src/hb-ot-cff1-std-str.hh" ] (
          Emit.moduleBlock "CffStandardStrings" (Emit.byteArrays "strings" cffStrings))
      output "CffPredefined.fs" cffSources (
          Emit.moduleBlock "CffPredefined" (
              String.Join(
                  "\n\n",
                  [ Emit.bytes "standardEncodingToCode" standardEncodingToCode
                    Emit.bytes "expertEncodingToCode" expertEncodingToCode
                    Emit.uint16s "expertCharsetToSid" expertCharsetToSid
                    Emit.uint16s "expertSubsetCharsetToSid" expertSubsetCharsetToSid
                    Emit.pairs "expertCharsetSidToGid" expertCharsetSidToGid
                    Emit.pairs "expertSubsetCharsetSidToGid" expertSubsetCharsetSidToGid
                    Emit.bytes "standardEncodingToSid" standardEncodingToSid ])))
      output "CmapMacRoman.fs" [ "src/hb-ot-cmap-table.hh" ] (
          Emit.moduleBlock "CmapMacRoman" (
              String.Join(
                  "\n",
                  [ Emit.bytePairs "mapping" macRoman
                    "/// <summary>Binary-searches the generated MacRoman mapping.</summary>"
                    "/// <param name=\"key\">The Unicode key.</param>"
                    "/// <param name=\"lo\">The first candidate.</param>"
                    "/// <param name=\"hi\">The last candidate.</param>"
                    "/// <returns>The MacRoman byte, or 0 when absent.</returns>"
                    "[<TailCall>]"
                    "let rec private search (key: uint16) (lo: int) (hi: int) : byte ="
                    "    match lo > hi with"
                    "    | true -> 0uy"
                    "    | false ->"
                    "        let mid = (lo + hi) / 2"
                    "        let struct (unicode, value) = mapping[mid]"
                    "        match key < unicode, key > unicode with"
                    "        | true, _ -> search key lo (mid - 1)"
                    "        | false, true -> search key (mid + 1) hi"
                    "        | false, false -> value"
                    ""
                    "/// <summary>Maps a Unicode value through the generated MacRoman pairs.</summary>"
                    "/// <param name=\"codepoint\">The Unicode value.</param>"
                    "/// <returns>The MacRoman byte, or 0 when absent.</returns>"
                    "let unicodeToMacRoman (codepoint: uint32) : byte ="
                    "    search (uint16 codepoint) 0 (mapping.Length - 1)" ])))
      output "ArabicPua.fs" [ "src/ArabicPUASimplified.txt"; "src/ArabicPUATraditional.txt" ] (
          Emit.moduleBlock "ArabicPua" (
              String.Join(
                  "\n",
                  [ Emit.pairs "simplifiedMap" (asPairs simplified)
                    ""
                    Emit.pairs "traditionalMap" (asPairs traditional)
                    ""
                    "/// <summary>Binary-searches one generated Arabic PUA map.</summary>"
                    "/// <param name=\"mapping\">The sorted map.</param>"
                    "/// <param name=\"codepoint\">The Unicode value.</param>"
                    "/// <param name=\"lo\">The first candidate.</param>"
                    "/// <param name=\"hi\">The last candidate.</param>"
                    "/// <returns>The PUA value, or 0 when absent.</returns>"
                    "[<TailCall>]"
                    "let rec private search (mapping: ImmutableArray<struct (uint16 * uint16)>) (codepoint: uint32) (lo: int) (hi: int) : uint16 ="
                    "    match lo > hi with"
                    "    | true -> 0us"
                    "    | false ->"
                    "        let mid = (lo + hi) / 2"
                    "        let struct (unicode, value) = mapping[mid]"
                    "        match codepoint < uint32 unicode, codepoint > uint32 unicode with"
                    "        | true, _ -> search mapping codepoint lo (mid - 1)"
                    "        | false, true -> search mapping codepoint (mid + 1) hi"
                    "        | false, false -> value"
                    ""
                    "/// <summary>Maps a Unicode value through the simplified Arabic PUA table.</summary>"
                    "/// <param name=\"codepoint\">The Unicode value.</param>"
                    "/// <returns>The PUA value, or 0 when absent.</returns>"
                    "let simplified (codepoint: uint32) : uint16 ="
                    "    search simplifiedMap codepoint 0 (simplifiedMap.Length - 1)"
                    ""
                    "/// <summary>Maps a Unicode value through the traditional Arabic PUA table.</summary>"
                    "/// <param name=\"codepoint\">The Unicode value.</param>"
                    "/// <returns>The PUA value, or 0 when absent.</returns>"
                    "let traditional (codepoint: uint32) : uint16 ="
                    "    search traditionalMap codepoint 0 (traditionalMap.Length - 1)" ])))
      output "Scripts.fs" [ "src/hb-script-list.h"; "src/hb-common.cc" ] (Scripts.generate (UpstreamSources.root ()))
      let ucdPath = Path.Combine(UpstreamSources.root (), "tools", "data", "ucd", "ucd.nounihan.grouped.zip")
      let struct (description, records) =
          match UcdXml.load ucdPath with
          | Ok loaded -> loaded
          | Error error -> failwith error
      output "UcdTables.fs" [ "tools/data/ucd/ucd.nounihan.grouped.zip"; "src/hb-script-list.h" ]
          (UcdGen.generate (UpstreamSources.root ()) description records)
      output "EmojiTables.fs" [ "tools/data/ucd/emoji-data.txt" ]
          (EmojiGen.generate (Path.Combine(UpstreamSources.root (), "tools", "data", "ucd", "emoji-data.txt")))
      output "OtTagTable.fs"
          [ "src/gen-tag-table.py"; "tools/data/vendored/languagetags.html"; "tools/data/vendored/language-subtag-registry" ]
          (TagGen.emit otTagData)
      output "IndicTable.fs"
          [ "src/gen-indic-table.py"; "src/hb-ot-shaper-indic.hh"; "src/hb-ot-shaper-indic-machine.hh"
            "src/hb-ot-shaper-khmer-machine.hh"; "src/hb-ot-shaper-myanmar-machine.hh"
            "tools/data/ucd/IndicSyllabicCategory.txt"; "tools/data/ucd/IndicPositionalCategory.txt"
            "tools/data/ucd/Blocks.txt" ]
          (IndicTable.generate (UpstreamSources.root ()))
      output "UseTable.fs"
          [ "src/gen-use-table.py"; "src/hb-ot-shaper-use-machine.hh"
            "src/ms-use/IndicSyllabicCategory-Additional.txt"; "src/ms-use/IndicPositionalCategory-Additional.txt"
            "tools/data/ucd/IndicSyllabicCategory.txt"; "tools/data/ucd/IndicPositionalCategory.txt"
            "tools/data/ucd/ArabicShaping.txt"; "tools/data/ucd/DerivedCoreProperties.txt"
            "tools/data/ucd/UnicodeData.txt"; "tools/data/ucd/Blocks.txt"; "tools/data/ucd/Scripts.txt" ]
          (UseTable.generate (UpstreamSources.root ()))
      output "ArabicTable.fs"
          [ "src/gen-arabic-table.py"; "src/hb-ot-shaper-arabic-table.hh"
            "tools/data/ucd/ArabicShaping.txt"; "tools/data/ucd/UnicodeData.txt"; "tools/data/ucd/Blocks.txt" ]
          (ArabicTable.generate (UpstreamSources.root ()))
      output "ArabicJoiningList.fs"
          [ "src/gen-arabic-joining-list.py"; "src/hb-ot-shaper-arabic-joining-list.hh"; "src/hb-script-list.h"
            "tools/data/ucd/ArabicShaping.txt"; "tools/data/ucd/Scripts.txt" ]
          (ArabicJoiningList.generate (UpstreamSources.root ()))
      output "ArabicWin1256.fs"
          [ "src/hb-ot-shaper-arabic-win1256.hh" ]
          (ArabicWin1256.generate (UpstreamSources.root ()))
      output "VowelConstraints.fs"
          [ "src/gen-vowel-constraints.py"; "src/hb-ot-shaper-vowel-constraints.hh"; "src/hb-script-list.h"
            "src/ms-use/IndicShapingInvalidCluster.txt"; "tools/data/ucd/Scripts.txt" ]
          (VowelConstraints.generate (UpstreamSources.root ()))
      output "AatFeatureMappings.fs"
          [ "src/hb-aat-layout.cc"; "src/hb-aat-layout.h" ]
          (AatMappings.generate (UpstreamSources.root ())) ] @ machineOutputs

/// <summary>The tools/data directory.</summary>
/// <returns>The path.</returns>
let private dataDirectory () =
    Path.Combine(UpstreamSources.root (), "tools", "data")

/// <summary>The committed manifest path.</summary>
/// <returns>The path.</returns>
let private manifestPath () =
    Path.Combine(dataDirectory (), "manifest.tsv")

/// <summary>Recreates the in-house emoji cluster test lines from emoji-test.txt.</summary>
/// <returns>The expected file text, or an error.</returns>
let private emojiTestText () : Result<string, string> =
    let input = Path.Combine(dataDirectory (), "ucd", "emoji-test.txt")
    let target = Path.Combine(UpstreamSources.upstream (), "test", "shape", "data", "in-house", "tests", "emoji-clusters.tests")
    match File.Exists input with
    | false -> Error $"missing emoji input: {input}"
    | true ->
        let lines =
            File.ReadAllLines input
            |> Array.choose (fun line ->
                let beforeComment = match line.IndexOf '#' with | -1 -> line | index -> line.Substring(0, index)
                let beforeSemicolon = match beforeComment.IndexOf ';' with | -1 -> beforeComment | index -> beforeComment.Substring(0, index)
                let sequence = beforeSemicolon.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
                match sequence.Length with
                | length when length < 2 -> None
                | _ -> Some sequence)
        let text =
            lines
            |> Array.map (fun sequence ->
                let codepoints = String.Join(",", sequence)
                let positions = String.Join("|", sequence |> Array.map (fun _ -> "1=0"))
                $"../fonts/AdobeBlank2.ttf;--no-glyph-names --no-positions --font-funcs=ot;{codepoints};[{positions}]")
            |> fun rows -> String.Join("\n", rows) + "\n"
        match File.Exists target with
        | false -> Error $"missing emoji test target: {target}"
        | true ->
            match File.ReadAllText target = text with
            | true -> Ok text
            | false -> Error "emoji-clusters.tests does not match emoji-test.txt"

/// <summary>Writes every generated file, or checks them without writing.</summary>
/// <param name="check">Whether to check only.</param>
/// <returns>Zero when the files are up to date or written.</returns>
let private run (check: bool) =
    let directory = Path.Combine(UpstreamSources.root (), "src", "Farfbuzz", "Generated")
    let outputs = generate ()
    let differing =
        outputs
        |> List.filter (fun (relative, text) ->
            let path = Path.Combine(directory, relative)
            not (File.Exists path) || File.ReadAllText path <> text)
    match check with
    | true ->
        differing
        |> List.iter (fun (relative, _) -> Console.Error.WriteLine $"    generated file differs: src/Farfbuzz/Generated/{relative}")
        let expectedNames = outputs |> List.map fst
        let extras =
            match Directory.Exists directory with
            | false -> []
            | true ->
                Directory.EnumerateFiles(directory, "*.fs")
                |> Seq.map Path.GetFileName
                |> Seq.filter (fun name -> not (List.contains name expectedNames))
                |> Seq.sort
                |> Seq.toList
        extras
        |> List.iter (fun name -> Console.Error.WriteLine $"    generated file is not written by the generator: src/Farfbuzz/Generated/{name}")
        let emojiError =
            match emojiTestText () with
            | Ok _ -> None
            | Error error -> Console.Error.WriteLine $"    {error}"; Some error
        let tagErrors = TagGen.verifyUpstream (UpstreamSources.root ()) (TagGen.build (UpstreamSources.root ()))
        tagErrors |> List.iter (fun error -> Console.Error.WriteLine $"    tag-table differs from upstream: {error}")
        match differing, extras, emojiError, tagErrors with
        | [], [], None, [] -> 0
        | _ -> 1
    | false ->
        Directory.CreateDirectory directory |> ignore
        outputs |> List.iter (fun (relative, text) -> File.WriteAllText(Path.Combine(directory, relative), text))
        Console.WriteLine $"    wrote {outputs.Length} generated files"
        0

/// <summary>Reads the manifest or prints the error.</summary>
/// <returns>The entries, or none.</returns>
let private readManifest () =
    match Manifest.read (manifestPath ()) with
    | Ok entries -> Some entries
    | Error error -> Console.Error.WriteLine $"    {error}"; None

/// <summary>Verifies every input and prints the count.</summary>
/// <returns>Zero on success.</returns>
let private verifyInputs () =
    match readManifest () with
    | None -> 2
    | Some entries ->
        match Fetch.verify (dataDirectory ()) entries with
        | Ok () -> Console.WriteLine $"    verified {entries.Length} entries"; 0
        | Error error -> Console.Error.WriteLine $"    {error}"; 2

/// <summary>Fetches missing inputs, optionally pinning their hashes.</summary>
/// <param name="pin">Whether to compute and write missing hashes.</param>
/// <returns>Zero on success.</returns>
let private fetchInputs (pin: bool) =
    match readManifest () with
    | None -> 2
    | Some entries ->
        match Fetch.fetchAll (dataDirectory ()) entries with
        | Error error -> Console.Error.WriteLine $"    {error}"; 2
        | Ok () ->
            match pin with
            | true ->
                let pinned =
                    entries
                    |> List.map (fun entry -> { entry with Sha256 = Fetch.sha256 (Fetch.localPath (dataDirectory ()) entry) })
                Manifest.write (manifestPath ()) pinned
                Console.WriteLine $"    pinned {pinned.Length} entries"
                0
            | false -> verifyInputs ()

/// <summary>Runs regen only after every input verifies.</summary>
/// <param name="check">Whether to check only.</param>
/// <returns>Zero on success.</returns>
let private verifiedRegen (check: bool) =
    match verifyInputs () with
    | 0 -> run check
    | code -> code

/// <summary>Runs the generator: <c>regen [--check]</c>.</summary>
/// <param name="argv">The command line.</param>
/// <returns>Zero on success.</returns>
[<EntryPoint>]
let main argv =
    match argv |> Array.toList with
    | [ "fetch" ] -> fetchInputs false
    | [ "fetch"; "--pin" ] -> fetchInputs true
    | [ "verify" ] -> verifyInputs ()
    | [ "regen" ] -> verifiedRegen false
    | [ "regen"; "--check" ] -> verifiedRegen true
    | _ ->
        Console.Error.WriteLine "usage: dotnet run --project tools/Farfbuzz.Gen -- fetch [--pin] | verify | regen [--check]"
        2
