module Farfbuzz.Gen.ArabicWin1256

open System
open System.Collections.Generic
open System.IO
open Farfbuzz.Gen

/// <summary>The upstream src directory.</summary>
let private sourceDirectory (root: string) = Path.Combine(root, "upstream", "harfbuzz", "src")

/// <summary>The lookup type and flag constants used by the table (src/hb-ot-shaper-arabic-win1256.hh:112, :137-138).</summary>
let private constants =
    Map.ofList [ "OT_LOOKUP_TYPE_SUBST_SINGLE", 1; "OT_LOOKUP_TYPE_SUBST_LIGATURE", 4; "OT_LOOKUP_FLAG_IGNORE_MARKS", 8 ]

/// <summary>One token of the table body.</summary>
type private Token =
    | TIdent of string
    | TNumber of int
    | TChar of char
    | TLParen
    | TRParen
    | TComma

/// <summary>One offset reference, <c>OT_OFFSET(From, To)</c>.</summary>
type private Offset = { From: string; To: string }

/// <summary>One top-level invocation of the table body.</summary>
type private Command =
    | ManifestCmd of (uint32 * string) list // tag and lookup name
    | LookupCmd of string * int * int * Offset list // name, type, flag, sublookup offsets
    | SingleSubCmd of string * int list * int list // name, from glyphs, to glyphs
    | LigatureSubCmd of string * int list * Offset list // name, first glyphs, ligature-set offsets
    | LigatureSetCmd of string * Offset list // name, offsets
    | LigatureCmd of string * int list * int // name, components, ligature glyph

/// <summary>Tokenizes the table body, skipping whitespace and block comments.</summary>
/// <param name="body">The text between <c>OT_TABLE_START</c> and <c>OT_TABLE_END</c>.</param>
/// <returns>The tokens.</returns>
let private tokenize (body: string) : Token list =
    let chars = body.ToCharArray()
    let tokens = ResizeArray<Token>()
    let mutable i = 0
    while i < chars.Length do
        let c = chars[i]
        if Char.IsWhiteSpace c then
            i <- i + 1
        elif c = '/' && i + 1 < chars.Length && chars[i + 1] = '*' then
            i <- i + 2
            while i + 1 < chars.Length && not (chars[i] = '*' && chars[i + 1] = '/') do
                i <- i + 1
            i <- i + 2
        elif Char.IsLetter c || c = '_' then
            let start = i
            while i < chars.Length && (Char.IsLetterOrDigit chars[i] || chars[i] = '_') do
                i <- i + 1
            tokens.Add (TIdent (body.Substring(start, i - start)))
        elif Char.IsDigit c then
            let start = i
            while i < chars.Length && Char.IsDigit chars[i] do
                i <- i + 1
            tokens.Add (TNumber (int (body.Substring(start, i - start))))
        elif c = '\'' then
            tokens.Add (TChar chars[i + 1])
            i <- i + 3
        elif c = '(' then
            tokens.Add TLParen
            i <- i + 1
        elif c = ')' then
            tokens.Add TRParen
            i <- i + 1
        elif c = ',' then
            tokens.Add TComma
            i <- i + 1
        else
            failwith $"unexpected character '{c}' in win1256 header"
    tokens |> List.ofSeq

/// <summary>A cursor over the token list.</summary>
type private Parser = { Tokens: Token array; mutable Pos: int }

/// <summary>Returns the next token without consuming it.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The next token.</returns>
let private peek (parser: Parser) = parser.Tokens[parser.Pos]

/// <summary>Consumes and returns the next token.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The consumed token.</returns>
let private advance (parser: Parser) =
    let token = parser.Tokens[parser.Pos]
    parser.Pos <- parser.Pos + 1
    token

/// <summary>Consumes an identifier.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The identifier.</returns>
let private expectIdent (parser: Parser) =
    match advance parser with
    | TIdent name -> name
    | _ -> failwith "expected identifier"

/// <summary>Consumes a left parenthesis.</summary>
/// <param name="parser">The parser.</param>
let private expectLParen (parser: Parser) =
    match advance parser with
    | TLParen -> ()
    | _ -> failwith "expected ("

/// <summary>Consumes a right parenthesis.</summary>
/// <param name="parser">The parser.</param>
let private expectRParen (parser: Parser) =
    match advance parser with
    | TRParen -> ()
    | _ -> failwith "expected )"

/// <summary>Consumes a comma.</summary>
/// <param name="parser">The parser.</param>
let private expectComma (parser: Parser) =
    match advance parser with
    | TComma -> ()
    | _ -> failwith "expected ,"

/// <summary>Parses one <c>G(number)</c> glyph id.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The glyph id.</returns>
let private parseGlyph (parser: Parser) =
    match advance parser with
    | TIdent "G" -> ()
    | _ -> failwith "expected G"
    expectLParen parser
    let value =
        match advance parser with
        | TNumber number -> number
        | _ -> failwith "expected glyph number"
    expectRParen parser
    value

/// <summary>Parses a run of <c>G(number)</c> glyph ids.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The glyph ids.</returns>
let private parseGlyphs (parser: Parser) : int list =
    let mutable acc = []
    let mutable done' = false
    while not done' do
        match peek parser with
        | TIdent "G" -> acc <- parseGlyph parser :: acc
        | _ -> done' <- true
    List.rev acc

/// <summary>Parses one <c>OT_OFFSET(from, to)</c>.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The offset reference.</returns>
let private parseOffset (parser: Parser) =
    match advance parser with
    | TIdent "OT_OFFSET" -> ()
    | _ -> failwith "expected OT_OFFSET"
    expectLParen parser
    let from = expectIdent parser
    expectComma parser
    let toName = expectIdent parser
    expectRParen parser
    { From = from; To = toName }

/// <summary>Parses a run of <c>OT_OFFSET(from, to)</c> references.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The offset references.</returns>
let private parseOffsets (parser: Parser) : Offset list =
    let mutable acc = []
    let mutable done' = false
    while not done' do
        match peek parser with
        | TIdent "OT_OFFSET" -> acc <- parseOffset parser :: acc
        | _ -> done' <- true
    List.rev acc

/// <summary>Parses one <c>OT_TAG('a','b','c','d')</c> into its packed tag value.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The tag value.</returns>
let private parseTag (parser: Parser) =
    match advance parser with
    | TIdent "OT_TAG" -> ()
    | _ -> failwith "expected OT_TAG"
    expectLParen parser
    let a =
        match advance parser with
        | TChar c -> byte c
        | _ -> failwith "expected character"
    expectComma parser
    let b =
        match advance parser with
        | TChar c -> byte c
        | _ -> failwith "expected character"
    expectComma parser
    let c =
        match advance parser with
        | TChar c -> byte c
        | _ -> failwith "expected character"
    expectComma parser
    let d =
        match advance parser with
        | TChar c -> byte c
        | _ -> failwith "expected character"
    expectRParen parser
    (uint32 a <<< 24) ||| (uint32 b <<< 16) ||| (uint32 c <<< 8) ||| uint32 d

/// <summary>Parses a lookup type or flag, resolving the named constants.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The value.</returns>
let private parseConstant (parser: Parser) =
    match advance parser with
    | TNumber number -> number
    | TIdent name ->
        match Map.tryFind name constants with
        | Some value -> value
        | None -> failwith $"unknown constant {name}"
    | _ -> failwith "expected constant"

/// <summary>Parses one top-level invocation of the table body.</summary>
/// <param name="parser">The parser.</param>
/// <returns>The command.</returns>
let private parseCommand (parser: Parser) : Command =
    let name = expectIdent parser
    expectLParen parser
    match name with
    | "MANIFEST" ->
        let items = ResizeArray<uint32 * string>()
        let mutable done' = false
        while not done' do
            match peek parser with
            | TIdent "MANIFEST_LOOKUP" ->
                advance parser |> ignore
                expectLParen parser
                let tag = parseTag parser
                expectComma parser
                let lookupName = expectIdent parser
                expectRParen parser
                items.Add (tag, lookupName)
            | TRParen -> done' <- true
            | _ -> failwith "expected MANIFEST_LOOKUP or )"
        expectRParen parser
        ManifestCmd (items |> List.ofSeq)
    | "OT_LOOKUP" ->
        let lookupName = expectIdent parser
        expectComma parser
        let lookupType = parseConstant parser
        expectComma parser
        let lookupFlag = parseConstant parser
        expectComma parser
        let offsets = parseOffsets parser
        expectRParen parser
        LookupCmd (lookupName, lookupType, lookupFlag, offsets)
    | "OT_SUBLOOKUP_SINGLE_SUBST_FORMAT2" ->
        let subName = expectIdent parser
        expectComma parser
        let fromGlyphs = parseGlyphs parser
        expectComma parser
        let toGlyphs = parseGlyphs parser
        expectRParen parser
        SingleSubCmd (subName, fromGlyphs, toGlyphs)
    | "OT_SUBLOOKUP_LIGATURE_SUBST_FORMAT1" ->
        let subName = expectIdent parser
        expectComma parser
        let firstGlyphs = parseGlyphs parser
        expectComma parser
        let setOffsets = parseOffsets parser
        expectRParen parser
        LigatureSubCmd (subName, firstGlyphs, setOffsets)
    | "OT_LIGATURE_SET" ->
        let setName = expectIdent parser
        expectComma parser
        let setOffsets = parseOffsets parser
        expectRParen parser
        LigatureSetCmd (setName, setOffsets)
    | "OT_LIGATURE" ->
        let ligName = expectIdent parser
        expectComma parser
        let components = parseGlyphs parser
        expectComma parser
        let ligGlyph = parseGlyph parser
        expectRParen parser
        LigatureCmd (ligName, components, ligGlyph)
    | other -> failwith $"unknown top-level macro {other}"

/// <summary>Parses the whole table body into commands.</summary>
/// <param name="body">The text between <c>OT_TABLE_START</c> and <c>OT_TABLE_END</c>.</param>
/// <returns>The commands.</returns>
let private parse (body: string) : Command list =
    let tokens = tokenize body |> List.toArray
    let parser = { Tokens = tokens; Pos = 0 }
    let commands = ResizeArray<Command>()
    while parser.Pos < parser.Tokens.Length do
        commands.Add (parseCommand parser)
    commands |> List.ofSeq

/// <summary>Extracts the table body text between the <c>OT_TABLE_START</c> and <c>OT_TABLE_END</c> invocations.</summary>
/// <param name="header">The whole header text.</param>
/// <returns>The table body.</returns>
let private tableBody (header: string) : string =
    let lines = header.Replace("\r", String.Empty).Split('\n')
    let start =
        lines
        |> Array.findIndex (fun line -> line.Trim() = "OT_TABLE_START")
    let finish =
        lines
        |> Array.findIndex (fun line -> line.Trim() = "OT_TABLE_END" && line.TrimEnd().Length > 0)
    match finish > start with
    | true -> String.Join("\n", lines[start + 1 .. finish - 1])
    | false -> failwith "OT_TABLE_END before OT_TABLE_START"

/// <summary>The labels a command defines, in source order, each with its byte size.</summary>
/// <param name="command">The command.</param>
/// <returns>The label names and sizes.</returns>
let private labelsOf (command: Command) : (string * int) list =
    match command with
    | ManifestCmd items -> [ "manifest", 2; "manifestData", 6 * items.Length ]
    | LookupCmd (name, _, _, offsets) -> [ name, 4; name + "SubLookupOffsetsArray", 2 + 2 * offsets.Length ]
    | SingleSubCmd (name, fromGlyphs, toGlyphs) ->
        [ name, 4; name + "Substitute", 2 + 2 * toGlyphs.Length; name + "Coverage", 2; name + "CoverageGlyphs", 2 + 2 * fromGlyphs.Length ]
    | LigatureSubCmd (name, firstGlyphs, setOffsets) ->
        [ name, 4
          name + "LigatureSetOffsetsArray", 2 + 2 * setOffsets.Length
          name + "Coverage", 2
          name + "CoverageGlyphs", 2 + 2 * firstGlyphs.Length ]
    | LigatureSetCmd (name, offsets) -> [ name, 2; name + "Data", 2 * offsets.Length ]
    | LigatureCmd (name, components, _) -> [ name, 2; name + "ComponentsArray", 2 + 2 * components.Length ]

/// <summary>Assigns each label its byte offset: labels are contiguous byte arrays in source order (no padding).</summary>
/// <param name="labels">The labels in source order.</param>
/// <returns>The label to offset map.</returns>
let private offsets (labels: (string * int) list) : Map<string, int> =
    let mutable position = 0
    let mutable map = Map.empty
    for name, size in labels do
        map <- map.Add (name, position)
        position <- position + size
    map

/// <summary>Appends a big-endian uint16.</summary>
/// <param name="out">The output.</param>
/// <param name="value">The value.</param>
let private emitUint16 (out: ResizeArray<byte>) (value: int) =
    out.Add (byte (value >>> 8))
    out.Add (byte (value &&& 0xFF))

/// <summary>Appends a big-endian uint32.</summary>
/// <param name="out">The output.</param>
/// <param name="value">The value.</param>
let private emitUint32 (out: ResizeArray<byte>) (value: uint32) =
    out.Add (byte (value >>> 24))
    out.Add (byte ((value >>> 16) &&& 0xFFu))
    out.Add (byte ((value >>> 8) &&& 0xFFu))
    out.Add (byte (value &&& 0xFFu))

/// <summary>Emits the full table bytes, resolving offsets against the label map.</summary>
/// <param name="commands">The commands.</param>
/// <param name="offsetMap">The label offsets.</param>
/// <returns>The table bytes.</returns>
let private emitBytes (commands: Command list) (offsetMap: Map<string, int>) : byte array =
    let out = ResizeArray<byte>()
    let emitOffset (offset: Offset) = emitUint16 out (offsetMap[offset.To] - offsetMap[offset.From])
    for command in commands do
        match command with
        | ManifestCmd items ->
            emitUint16 out items.Length
            for tag, lookupName in items do
                emitUint32 out tag
                emitUint16 out (offsetMap[lookupName] - offsetMap["manifest"])
        | LookupCmd (_, lookupType, lookupFlag, offsets) ->
            emitUint16 out lookupType
            emitUint16 out lookupFlag
            emitUint16 out offsets.Length
            for offset in offsets do
                emitOffset offset
        | SingleSubCmd (name, fromGlyphs, toGlyphs) ->
            emitUint16 out 2
            emitUint16 out (offsetMap[name + "Coverage"] - offsetMap[name])
            emitUint16 out toGlyphs.Length
            for glyph in toGlyphs do
                emitUint16 out glyph
            emitUint16 out 1
            emitUint16 out fromGlyphs.Length
            for glyph in fromGlyphs do
                emitUint16 out glyph
        | LigatureSubCmd (name, firstGlyphs, setOffsets) ->
            emitUint16 out 1
            emitUint16 out (offsetMap[name + "Coverage"] - offsetMap[name])
            emitUint16 out setOffsets.Length
            for offset in setOffsets do
                emitOffset offset
            emitUint16 out 1
            emitUint16 out firstGlyphs.Length
            for glyph in firstGlyphs do
                emitUint16 out glyph
        | LigatureSetCmd (_, offsets) ->
            emitUint16 out offsets.Length
            for offset in offsets do
                emitOffset offset
        | LigatureCmd (_, components, ligGlyph) ->
            emitUint16 out ligGlyph
            emitUint16 out (components.Length + 1)
            for glyph in components do
                emitUint16 out glyph
    out.ToArray()

/// <summary>Builds the generated ArabicWin1256.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    let header = File.ReadAllText (Path.Combine(sourceDirectory root, "hb-ot-shaper-arabic-win1256.hh"))
    let commands = parse (tableBody header)
    let labels = commands |> List.collect labelsOf
    let offsetMap = offsets labels
    let bytes = emitBytes commands offsetMap
    let manifest =
        commands
        |> List.collect (fun command ->
            match command with
            | ManifestCmd items -> items
            | _ -> [])
        |> List.map (fun (tag, lookupName) -> struct (tag, offsetMap[lookupName] - offsetMap["manifest"]))
    let byteItems = bytes |> Array.map (fun value -> $"{value}uy") |> String.concat "; "
    let manifestItems = manifest |> List.map (fun struct (tag, offset) -> $"struct (0x{tag:X8}u<tag>, {offset})") |> String.concat "; "
    [ "module internal ArabicWin1256 ="
      "    /// <summary>The Windows-1256 Arabic shaping lookups, <c>arabic_win1256_gsub_lookups</c> (src/hb-ot-shaper-arabic-win1256.hh:174-297).</summary>"
      "    let bytes : ImmutableArray<byte> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {byteItems} |])"
      "    /// <summary>The manifest records, each a feature tag and its lookup offset within <see cref=\"bytes\"/> (src/hb-ot-shaper-arabic-fallback.hh:240-248).</summary>"
      "    let manifest : ImmutableArray<struct (Tag * int)> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {manifestItems} |])" ]
    |> String.concat "\n"
