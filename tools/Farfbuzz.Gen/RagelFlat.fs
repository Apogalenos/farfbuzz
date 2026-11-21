module Farfbuzz.Gen.RagelFlat

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen

/// <summary>One statement of a generated action body, before it is emitted.</summary>
type RagelOpSyntax =
    | TsFromP
    | TsReset
    | TeFromNextP
    | TeFromP
    | PBack
    | PFromTe
    | ActSet of act: int
    | FoundSyllable of syllableType: int * broken: bool

/// <summary>One numbered action body, before it is emitted.</summary>
type RagelActionSyntax =
    | Ops of RagelOpSyntax list
    | ActSwitch of byAct: (int * RagelOpSyntax list) list

/// <summary>Everything one <c>-F1</c> machine header declares, in source order.</summary>
type FlatMachine =
    { Name: string
      Arrays: Map<string, struct (string * int64 list)>
      Constants: Map<string, int>
      Exports: (string * int) list
      SyllableTypes: string list
      Actions: Map<int, RagelActionSyntax>
      FromState: Map<int, RagelActionSyntax>
      ToState: Map<int, RagelActionSyntax> }

/// <summary>Converts an upstream <c>snake_case</c> identifier to PascalCase.</summary>
/// <param name="name">The upstream identifier.</param>
/// <returns>The F# name.</returns>
let private pascalCase (name: string) =
    name.Split('_')
    |> Array.map (fun part ->
        match part.Length with
        | 0 -> String.Empty
        | _ -> string (Char.ToUpperInvariant part[0]) + part.Substring(1).ToLowerInvariant())
    |> String.concat ""

/// <summary>Reads the <c>#define …_ex_*</c> category exports in file order.</summary>
/// <param name="text">The header text.</param>
/// <param name="name">The machine name.</param>
/// <returns>The export names and byte values.</returns>
let private exports (text: string) (name: string) : (string * int) list =
    let pattern = Regex($@"#define\s+{name}_syllable_machine_ex_(\w+)\s+(\d+)u?")
    pattern.Matches text
    |> Seq.map (fun m -> m.Groups[1].Value, int m.Groups[2].Value)
    |> Seq.toList

/// <summary>Reads the syllable-type enum entries in declaration order.</summary>
/// <param name="text">The header text.</param>
/// <param name="name">The machine name.</param>
/// <returns>The enum identifiers.</returns>
let private syllableTypes (text: string) (name: string) : string list =
    let header = $"enum {name}_syllable_type_t"
    let start = text.IndexOf header
    match start < 0 with
    | true -> failwith $"missing enum in {name}"
    | false ->
        let openBrace = text.IndexOf('{', start)
        let closeBrace = text.IndexOf('}', openBrace)
        text.Substring(openBrace + 1, closeBrace - openBrace - 1).Split(',')
        |> Array.map (fun part -> part.Trim())
        |> Array.filter (fun part -> part.Length > 0)
        |> Array.toList

/// <summary>Reads the four machine constants <c>start</c>, <c>first_final</c>, <c>error</c> and <c>en_main</c>.</summary>
/// <param name="text">The header text.</param>
/// <param name="name">The machine name.</param>
/// <returns>The constants.</returns>
let private constants (text: string) (name: string) : Map<string, int> =
    [ "start"; "first_final"; "error"; "en_main" ]
    |> List.map (fun constant ->
        let pattern = Regex($@"{name}_syllable_machine_{constant}\s*=\s*(-?\d+);")
        let found = pattern.Match text
        match found.Success with
        | false -> failwith $"missing constant {constant} in {name}"
        | true -> constant, int found.Groups[1].Value)
    |> Map.ofList

/// <summary>The kinds of the nine fixed arrays, in Ragel declaration order.</summary>
let private arrayKinds = [ "trans_keys"; "key_spans"; "index_offsets"; "indicies"; "trans_targs"; "trans_actions"; "to_state_actions"; "from_state_actions"; "eof_trans" ]

/// <summary>Reads one array declaration: its declared C type and its values in source order.</summary>
/// <param name="text">The header text.</param>
/// <param name="name">The machine name.</param>
/// <param name="kind">The array kind.</param>
/// <returns>The C type and the values.</returns>
let private array (text: string) (name: string) (kind: string) : struct (string * int64 list) =
    let pattern = Regex($@"static const (unsigned char|char|short) _{name}_syllable_machine_{kind}\[\]\s*=\s*\{{")
    let found = pattern.Match text
    match found.Success with
    | false -> failwith $"missing array {kind} in {name}"
    | true ->
        let ctype = found.Groups[1].Value
        let openBrace = found.Index + found.Length - 1
        let closeBrace = text.IndexOf("};", openBrace)
        let body = text.Substring(openBrace + 1, closeBrace - openBrace - 1)
        let values =
            Regex.Matches(body, @"-?\d+")
            |> Seq.map (fun value -> int64 value.Value)
            |> Seq.toList
        struct (ctype, values)

/// <summary>Returns the index of the brace that closes the brace at <paramref name="openIndex"/>.</summary>
/// <param name="text">The text.</param>
/// <param name="openIndex">The index of the opening brace.</param>
/// <returns>The closing brace index, or -1 when unbalanced.</returns>
let private matchingClose (text: string) (openIndex: int) : int =
    let mutable depth = 0
    let mutable index = openIndex
    let mutable result = -1
    while index < text.Length && result < 0 do
        match text[index] with
        | '{' -> depth <- depth + 1
        | '}' ->
            depth <- depth - 1
            match depth with
            | 0 -> result <- index
            | _ -> ()
        | _ -> ()
        index <- index + 1
    result

/// <summary>Returns the body of one exec switch block.</summary>
/// <param name="text">The header text.</param>
/// <param name="name">The machine name.</param>
/// <param name="kind">The switch kind.</param>
/// <param name="indexVariable">The index variable inside the brackets.</param>
/// <returns>The text between the switch's braces.</returns>
let private switchBody (text: string) (name: string) (kind: string) (indexVariable: string) : string =
    let pattern = Regex($@"switch\s*\(\s*_{name}_syllable_machine_{kind}\[{indexVariable}\]\s*\)")
    let found = pattern.Match text
    match found.Success with
    | false -> failwith $"missing switch {kind} in {name}"
    | true ->
        let openBrace = text.IndexOf('{', found.Index)
        let closeBrace = matchingClose text openBrace
        text.Substring(openBrace + 1, closeBrace - openBrace - 1)

/// <summary>Splits a switch body into its top-level <c>case N:</c> … <c>break;</c> entries.</summary>
/// <param name="body">The switch body between braces.</param>
/// <returns>The case ids and their bodies.</returns>
let private switchCases (body: string) : (int * string) list =
    let cases = ResizeArray<int * string>()
    let mutable depth = 0
    let mutable index = 0
    let mutable caseId = -1
    let mutable caseStart = -1
    while index < body.Length do
        match depth with
        | 0 when body.Substring(index).StartsWith "case" && (index + 4 = body.Length || Char.IsWhiteSpace body[index + 4]) ->
            let mutable cursor = index + 4
            while cursor < body.Length && Char.IsWhiteSpace body[cursor] do
                cursor <- cursor + 1
            let digits = cursor
            while cursor < body.Length && Char.IsDigit body[cursor] do
                cursor <- cursor + 1
            caseId <- int (body.Substring(digits, cursor - digits))
            caseStart <- cursor + 1
            index <- cursor + 1
        | 0 when body.Substring(index).StartsWith "break;" ->
            match caseId >= 0 with
            | true -> cases.Add(caseId, body.Substring(caseStart, index - caseStart))
            | false -> ()
            caseId <- -1
            caseStart <- -1
            index <- index + 6
        | _ ->
            match body[index] with
            | '{' -> depth <- depth + 1
            | '}' -> depth <- depth - 1
            | _ -> ()
            index <- index + 1
    List.ofSeq cases

/// <summary>Removes <c>#line</c> directives from an action body.</summary>
/// <param name="text">The action body.</param>
/// <returns>The body without directives.</returns>
let private stripLines (text: string) =
    text.Split('\n')
    |> Array.filter (fun line -> not (Regex.IsMatch(line, @"^\s*#line")))
    |> String.concat "\n"

/// <summary>Resolves a syllable-type enum identifier to its index.</summary>
/// <param name="syllableTypes">The enum identifiers.</param>
/// <param name="identifier">The identifier to resolve.</param>
/// <returns>The index, or an error.</returns>
let private resolveType (syllableTypes: string list) (identifier: string) : Result<int, string> =
    match syllableTypes |> List.tryFindIndex (fun item -> item = identifier) with
    | Some index -> Ok index
    | None -> Error $"unknown syllable type {identifier}"

/// <summary>Collects a list of results, stopping at the first error.</summary>
/// <param name="results">The results.</param>
/// <returns>The values or the first error.</returns>
let private allOk (results: Result<'a, string> list) : Result<'a list, string> =
    let mutable values = []
    let mutable error = None
    for result in results do
        match error, result with
        | Some _, _ -> ()
        | None, Ok value -> values <- value :: values
        | None, Error message -> error <- Some message
    match error with
    | Some message -> Error message
    | None -> Ok (List.rev values)

/// <summary>Parses one action body into its statement list.</summary>
/// <param name="text">The action body, directives already removed.</param>
/// <param name="syllableTypes">The syllable-type enum identifiers.</param>
/// <returns>The operations.</returns>
let private parseStatements (text: string) (syllableTypes: string list) : Result<RagelOpSyntax list, string> =
    let ops = ResizeArray<RagelOpSyntax>()
    let mutable pos = 0
    let skip () =
        while pos < text.Length && Char.IsWhiteSpace text[pos] do
            pos <- pos + 1
    let startsWith (token: string) =
        text.Length - pos >= token.Length && text.Substring(pos, token.Length) = token
    let brokenToken = "buffer->scratch_flags |= HB_BUFFER_SCRATCH_FLAG_HAS_BROKEN_SYLLABLE;"
    let rec statements () : Result<unit, string> =
        skip ()
        match pos >= text.Length || text[pos] = '}' with
        | true -> Ok ()
        | false ->
            match text[pos] with
            | '{' ->
                pos <- pos + 1
                match statements () with
                | Ok () ->
                    skip ()
                    match pos < text.Length && text[pos] = '}' with
                    | true ->
                        pos <- pos + 1
                        statements ()
                    | false -> Error "unterminated brace in action"
                | Error error -> Error error
            | _ -> parseOp ()
    and parseOp () : Result<unit, string> =
        match () with
        | _ when startsWith "ts = p;" -> pos <- pos + 7; ops.Add TsFromP; statements ()
        | _ when startsWith "ts = 0;" -> pos <- pos + 7; ops.Add TsReset; statements ()
        | _ when startsWith "te = p+1;" -> pos <- pos + 9; ops.Add TeFromNextP; statements ()
        | _ when startsWith "te = p;" -> pos <- pos + 7; ops.Add TeFromP; statements ()
        | _ when startsWith "p--;" -> pos <- pos + 4; ops.Add PBack; statements ()
        | _ when startsWith "p = ((te))-1;" -> pos <- pos + 13; ops.Add PFromTe; statements ()
        | _ when startsWith "act = " ->
            pos <- pos + 6
            let start = pos
            while pos < text.Length && Char.IsDigit text[pos] do
                pos <- pos + 1
            match pos = start || pos >= text.Length || text[pos] <> ';' with
            | true -> Error "malformed act = N;"
            | false ->
                ops.Add (ActSet (int (text.Substring(start, pos - start))))
                pos <- pos + 1
                statements ()
        | _ when startsWith "found_syllable" ->
            pos <- pos + 14
            skip ()
            match pos < text.Length && text[pos] = '(' with
            | false -> Error "malformed found_syllable"
            | true ->
                pos <- pos + 1
                skip ()
                let start = pos
                while pos < text.Length && (Char.IsLetterOrDigit text[pos] || text[pos] = '_') do
                    pos <- pos + 1
                let identifier = text.Substring(start, pos - start)
                skip ()
                match pos < text.Length && text[pos] = ')' with
                | false -> Error "malformed found_syllable"
                | true ->
                    pos <- pos + 1
                    skip ()
                    match pos < text.Length && text[pos] = ';' with
                    | false -> Error "malformed found_syllable"
                    | true ->
                        pos <- pos + 1
                        skip ()
                        let broken = startsWith brokenToken
                        match broken with
                        | true -> pos <- pos + brokenToken.Length
                        | false -> ()
                        match resolveType syllableTypes identifier with
                        | Ok index -> ops.Add (FoundSyllable (index, broken)); statements ()
                        | Error error -> Error error
        | _ -> Error $"unrecognized action statement: {text.Substring(pos, min 30 (text.Length - pos))}"
    statements () |> Result.map (fun () -> List.ofSeq ops)

/// <summary>Parses one action body, dispatching the <c>switch (act)</c> form.</summary>
/// <param name="text">The action body.</param>
/// <param name="syllableTypes">The syllable-type enum identifiers.</param>
/// <returns>The action.</returns>
let private parseActionBody (text: string) (syllableTypes: string list) : Result<RagelActionSyntax, string> =
    let cleaned = stripLines text
    let trimmed = cleaned.Trim()
    match trimmed.IndexOf "switch" >= 0 with
    | true ->
        let openBrace = trimmed.IndexOf('{', trimmed.IndexOf "switch")
        let closeBrace = matchingClose trimmed openBrace
        let body = trimmed.Substring(openBrace + 1, closeBrace - openBrace - 1)
        switchCases body
        |> List.map (fun (id, caseBody) ->
            parseStatements (stripLines caseBody) syllableTypes |> Result.map (fun ops -> id, ops))
        |> allOk
        |> Result.map ActSwitch
    | false ->
        parseStatements trimmed syllableTypes
        |> Result.map (fun ops -> match ops.IsEmpty with | true -> Ops [] | false -> Ops ops)

/// <summary>Parses the cases of one exec switch into an action map.</summary>
/// <param name="body">The switch body.</param>
/// <param name="syllableTypes">The syllable-type enum identifiers.</param>
/// <returns>The action map.</returns>
let private parseSwitchActions (body: string) (syllableTypes: string list) : Result<Map<int, RagelActionSyntax>, string> =
    switchCases body
    |> List.map (fun (id, caseBody) ->
        parseActionBody caseBody syllableTypes |> Result.map (fun action -> id, action))
    |> allOk
    |> Result.map Map.ofList

/// <summary>Validates an array's values against its C type.</summary>
/// <param name="ctype">The declared C type.</param>
/// <param name="kind">The array kind.</param>
/// <param name="values">The values.</param>
/// <returns>An error, or none.</returns>
let private validateArray (ctype: string) (kind: string) (values: int64 list) : string option =
    let limit = match ctype with | "short" -> 65535L | _ -> 255L
    match values |> List.tryFind (fun value -> value < 0L || value > limit) with
    | Some value -> Some $"{kind} has a value {value} outside {ctype} range"
    | None -> None

/// <summary>Parses one machine header into its flat tables.</summary>
/// <param name="machineHh">The path of the <c>-F1</c> header.</param>
/// <param name="name">The machine name, for example <c>indic</c>.</param>
/// <returns>The flat machine, or an error.</returns>
let parse (machineHh: string) (name: string) : Result<FlatMachine, string> =
    let text = File.ReadAllText machineHh
    let consts = constants text name
    let exportsList = exports text name
    let types = syllableTypes text name
    let arrays =
        arrayKinds
        |> List.map (fun kind -> kind, array text name kind)
        |> Map.ofList
    let fromActions = parseSwitchActions (switchBody text name "from_state_actions" "cs") types
    let transActions = parseSwitchActions (switchBody text name "trans_actions" "_trans") types
    let toActions = parseSwitchActions (switchBody text name "to_state_actions" "cs") types
    let struct (transKeysCtype, transKeysRaw) = arrays["trans_keys"]
    let struct (_, keySpans) = arrays["key_spans"]
    let struct (_, indexOffsets) = arrays["index_offsets"]
    let struct (_, transTargs) = arrays["trans_targs"]
    let struct (_, transActionsValues) = arrays["trans_actions"]
    let struct (_, indicies) = arrays["indicies"]
    let struct (_, eofTrans) = arrays["eof_trans"]
    let states = keySpans.Length
    let transitions = transTargs.Length
    let issues = ResizeArray<string>()
    if consts["start"] <> consts["first_final"] || consts["start"] <> consts["en_main"] then
        issues.Add "start, first_final and en_main differ"
    if consts["error"] <> -1 then
        let errorValue = consts["error"]
        issues.Add $"error is {errorValue}, not -1"
    if transKeysRaw.Length <> 2 * states + 1 then
        issues.Add $"trans_keys has {transKeysRaw.Length} entries, not {2 * states + 1}"
    match List.tryLast transKeysRaw with
    | Some 0L -> ()
    | _ -> issues.Add "trans_keys does not end in the 0 sentinel"
    if indexOffsets.Length <> states then
        issues.Add $"index_offsets has {indexOffsets.Length} entries, not {states}"
    if transActionsValues.Length <> transitions then
        issues.Add $"trans_actions has {transActionsValues.Length} entries, not {transitions}"
    if not (indicies |> List.forall (fun value -> value < int64 transitions)) then
        issues.Add "an indicies value is not below the transition count"
    if not (eofTrans |> List.forall (fun value -> value <= int64 transitions)) then
        issues.Add "an eof_trans value exceeds the transition count"
    match transActions with
    | Ok actions ->
        if not (transActionsValues |> List.forall (fun value -> value = 0L || Map.containsKey (int value) actions)) then
            issues.Add "a trans_actions id has no case"
    | Error error -> issues.Add error
    if not (text.Contains "_keys[0] <=" && text.Contains "- _keys[0] : _slen") then
        issues.Add "the exec text does not use the -F1 key expression"
    for kind in arrayKinds do
        let struct (ctype, values) = arrays[kind]
        match validateArray ctype kind values with
        | Some message -> issues.Add message
        | None -> ()
    match fromActions with
    | Ok actions ->
        match Map.toList actions with
        | [ (_, Ops [ TsFromP ]) ] -> ()
        | _ -> issues.Add "the from-state list is not exactly [TsFromP]"
    | Error error -> issues.Add error
    match toActions with
    | Ok actions ->
        match Map.toList actions with
        | [ (_, Ops [ TsReset ]) ] -> ()
        | _ -> issues.Add "the to-state list is not exactly [TsReset]"
    | Error error -> issues.Add error
    match issues.Count with
    | 0 ->
        let transKeysDropped = transKeysRaw |> List.take (transKeysRaw.Length - 1)
        let arrays = Map.add "trans_keys" (struct (transKeysCtype, transKeysDropped)) arrays
        Ok
            { Name = name
              Arrays = arrays
              Constants = consts
              Exports = exportsList
              SyllableTypes = types
              Actions = (match transActions with | Ok actions -> actions | Error _ -> Map.empty)
              FromState = (match fromActions with | Ok actions -> actions | Error _ -> Map.empty)
              ToState = (match toActions with | Ok actions -> actions | Error _ -> Map.empty) }
    | _ -> Error issues[0]

/// <summary>Formats one operation as an F# expression.</summary>
/// <param name="op">The operation.</param>
/// <returns>The expression.</returns>
let private emitOp (op: RagelOpSyntax) : string =
    match op with
    | TsFromP -> "RagelOp.TsFromP"
    | TsReset -> "RagelOp.TsReset"
    | TeFromNextP -> "RagelOp.TeFromNextP"
    | TeFromP -> "RagelOp.TeFromP"
    | PBack -> "RagelOp.PBack"
    | PFromTe -> "RagelOp.PFromTe"
    | ActSet act -> $"RagelOp.ActSet {act}uy"
    | FoundSyllable (syllableType, broken) ->
        let brokenText = match broken with | true -> "true" | false -> "false"
        $"RagelOp.FoundSyllable ({syllableType}uy, {brokenText})"

/// <summary>Formats an operation list as an F# <c>ImmutableArray&lt;RagelOp&gt;</c> expression.</summary>
/// <param name="ops">The operations.</param>
/// <returns>The expression.</returns>
let private emitOpsArray (ops: RagelOpSyntax list) : string =
    match ops with
    | [] -> "ImmutableArray<RagelOp>.Empty"
    | _ ->
        let opsText = ops |> List.map emitOp |> String.concat "; "
        $"Sealed.ofSpan (ReadOnlySpan [| {opsText} |])"

/// <summary>Formats one action as an F# <c>RagelAction</c> expression.</summary>
/// <param name="action">The action, or none for the implicit no-op.</param>
/// <returns>The expression.</returns>
let private emitAction (action: RagelActionSyntax option) : string =
    match action with
    | None -> "RagelAction.NoAction"
    | Some (Ops ops) -> $"RagelAction.Ops ({emitOpsArray ops})"
    | Some (ActSwitch byAct) ->
        let maxAct = byAct |> List.map fst |> List.max
        let cases =
            [ for act in 0 .. maxAct ->
                match byAct |> List.tryFind (fun (id, _) -> id = act) with
                | Some (_, ops) -> emitOpsArray ops
                | None -> "ImmutableArray<RagelOp>.Empty" ]
        let casesText = cases |> String.concat "; "
        $"RagelAction.ActSwitch (Sealed.ofSpan (ReadOnlySpan [| {casesText} |]))"

/// <summary>Emits the generated F# file for one machine.</summary>
/// <param name="flat">The parsed machine.</param>
/// <returns>The file text.</returns>
let emit (flat: FlatMachine) : string =
    let moduleName = pascalCase flat.Name + "Machine"
    let indent (text: string) = "    " + text.Replace("\n", "\n    ")
    let exportLiterals =
        flat.Exports
        |> List.map (fun (name, value) -> $"[<Literal>]\nlet {name} = {value}uy")
        |> String.concat "\n"
    let caseNames = flat.SyllableTypes |> List.map pascalCase
    let union =
        [ "[<Struct>]"
          "[<NoEquality; NoComparison>]"
          "type internal SyllableType ="
          yield! caseNames |> List.map (fun case -> $"    | {case}") ]
        |> String.concat "\n"
    let ofSyllable =
        [ "/// <summary>Maps a syllable byte's low nibble to its syllable type.</summary>"
          "/// <param name=\"syllable\">The syllable byte, whose high nibble is the serial.</param>"
          "/// <returns>The syllable type.</returns>"
          "let ofSyllable (syllable: byte) : SyllableType ="
          "    match syllable &&& 0x0Fuy with"
          yield! caseNames |> List.mapi (fun index case -> $"    | {index}uy -> {case}")
          "    | _ -> failwith \"invalid syllable type\"" ]
        |> String.concat "\n"
    let array (kind: string) (camel: string) (element: string) (suffix: string) =
        let struct (_, values) = flat.Arrays[kind]
        let items = values |> List.map (fun value -> $"{value}{suffix}") |> String.concat "; "
        $"let private {camel} : ImmutableArray<{element}> =\n    Sealed.ofSpan (ReadOnlySpan [| {items} |])"
    let boolArray (camel: string) (values: int64 list) (id: int) =
        let items = values |> List.map (fun value -> match int value = id with | true -> "true" | false -> "false") |> String.concat "; "
        $"let private {camel} : ImmutableArray<bool> =\n    Sealed.ofSpan (ReadOnlySpan [| {items} |])"
    let fromId = flat.FromState |> Map.toList |> List.head |> fst
    let toId = flat.ToState |> Map.toList |> List.head |> fst
    let struct (_, fromValues) = flat.Arrays["from_state_actions"]
    let struct (_, toValues) = flat.Arrays["to_state_actions"]
    let actionsArray =
        let maxAction = flat.Actions |> Map.toSeq |> Seq.map fst |> Seq.max
        let entries =
            [ for id in 0 .. maxAction -> emitAction (Map.tryFind id flat.Actions) ]
        let items = entries |> String.concat "; "
        $"let private actions : ImmutableArray<RagelAction> =\n    Sealed.ofSpan (ReadOnlySpan [| {items} |])"
    let machine =
        let startValue = flat.Constants["start"]
        [ "let machine : RagelMachine ="
          "    { TransKeys = transKeys"
          "      KeySpans = keySpans"
          "      IndexOffsets = indexOffsets"
          "      Indicies = indicies"
          "      TransTargs = transTargs"
          "      TransActions = transActions"
          "      SetsTsOnEntry = setsTsOnEntry"
          "      ResetsTsOnExit = resetsTsOnExit"
          "      EofTrans = eofTrans"
          "      Actions = actions"
          $"      Start = {startValue} }}" ]
        |> String.concat "\n"
    let members =
        [ exportLiterals
          union
          ofSyllable
          array "trans_keys" "transKeys" "byte" "uy"
          array "key_spans" "keySpans" "byte" "uy"
          array "index_offsets" "indexOffsets" "uint16" "us"
          array "indicies" "indicies" "byte" "uy"
          array "trans_targs" "transTargs" "byte" "uy"
          array "trans_actions" "transActions" "byte" "uy"
          boolArray "setsTsOnEntry" fromValues fromId
          boolArray "resetsTsOnExit" toValues toId
          array "eof_trans" "eofTrans" "uint16" "us"
          actionsArray
          machine ]
        |> List.map indent
        |> String.concat "\n\n"
    "module internal " + moduleName + " =\n\n" + members
