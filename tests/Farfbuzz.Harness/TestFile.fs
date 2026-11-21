module Farfbuzz.Harness.TestFile

open System
open System.IO

/// <summary>Where a test's font lives.</summary>
type FontRef =
    | Relative of path: string                       // resolved against the .tests file's folder
    | Absolute of path: string * sha1: string voption // a system font, run only when present with that hash

/// <summary>What a test expects.</summary>
type Expected =
    | Glyphs of text: string  // the exact hb-shape output line
    | Any                     // '*': the command must merely run

/// <summary>The backend kinds a directive or option can pin, as run-tests.py names them.</summary>
type BackendKind =
    | Shaper
    | FaceLoader
    | FontFuncs

/// <summary>An <c>@kind=a,b</c> (keep those) or <c>@kind-=a,b</c> (drop those) directive.</summary>
type Directive = { Kind: BackendKind; Exclude: bool; Values: string list }

/// <summary>The four fields of a test line.</summary>
type CaseFields =
    { Font: FontRef
      Options: string list  // hb-shape options, split on white space as run-tests.py does
      Unicodes: string
      Expected: Expected }

/// <summary>One line of a .tests file, classified as run-tests.py classifies it.</summary>
type TestLine =
    | Case of line: int * fields: CaseFields
    | CommentedCase of line: int * fields: CaseFields  // '#' directly followed by a case: parsed, never run
    | Directive of line: int * directive: Directive
    | Ignored of line: int                             // '# ' comments and blank lines
    | Malformed of line: int * reason: string          // run-tests.py would crash on it

/// <summary>Names a backend kind as directives and options spell it.</summary>
/// <param name="kind">The kind.</param>
/// <returns>The singular spelling.</returns>
let kindName (kind: BackendKind) =
    match kind with
    | Shaper -> "shaper"
    | FaceLoader -> "face-loader"
    | FontFuncs -> "font-funcs"

/// <summary>Recognises the kind a directive starts with, accepting singular and plural spellings.</summary>
/// <param name="text">The directive text without its '@'.</param>
/// <returns>The kind, or none.</returns>
let directiveKind (text: string) =
    [ Shaper; FaceLoader; FontFuncs ] |> List.tryFind (fun kind -> text.StartsWith(kindName kind, StringComparison.Ordinal))

/// <summary>Parses a directive line, <c>@shapers=ot,coretext # comment</c>.</summary>
/// <param name="number">The line number.</param>
/// <param name="text">The line, starting with '@'.</param>
/// <returns>The classified line.</returns>
let parseDirective (number: int) (text: string) =
    let body = (text.Split('#')[0]).Trim().Substring 1
    match directiveKind body, body.Split('=') with
    | Some kind, [| command; values |] ->
        Directive (number, { Kind = kind; Exclude = command.EndsWith '-'; Values = values.Trim().Split(',') |> Array.toList })
    | Some _, _ -> Malformed (number, "a directive needs exactly one =")
    | None, _ -> Malformed (number, $"unrecognised directive {body}")

/// <summary>Splits a font field into a relative path or an absolute path with an optional SHA-1.</summary>
/// <param name="field">The font field.</param>
/// <returns>The font reference.</returns>
let parseFont (field: string) =
    match field.StartsWith '/' || field.StartsWith "\"/" with
    | false -> Relative field
    | true ->
        match field.Split('@') with
        | [| path |] -> Absolute (path, ValueNone)
        | parts -> Absolute (parts[0], ValueSome parts[1])

/// <summary>Parses the four ';'-separated fields of a case.</summary>
/// <param name="text">The stripped line.</param>
/// <returns>The fields, or why run-tests.py would fail on the line.</returns>
let parseFields (text: string) =
    match text.Split(';') with
    | [| font; options; unicodes; expected |] ->
        Ok { Font = parseFont font
             Options = options.Split([| ' '; '\t'; '\n'; '\r'; '\f'; '\v' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
             Unicodes = unicodes
             Expected = if expected = "*" then Any else Glyphs expected }
    | fields -> Error $"{fields.Length} fields instead of 4"

/// <summary>Classifies one line exactly as run-tests.py's loop does.</summary>
/// <param name="number">The 1-based line number.</param>
/// <param name="raw">The raw line.</param>
/// <returns>The classified line.</returns>
let parseLine (number: int) (raw: string) : TestLine =
    let commented = raw.StartsWith '#'
    let afterHash = if commented then raw.Substring 1 else raw
    match commented && afterHash.StartsWith ' ', afterHash.Trim() with
    | true, _ -> Ignored number
    | false, stripped when stripped.Length = 0 -> Ignored number
    | false, stripped when stripped.StartsWith '@' -> parseDirective number stripped
    | false, stripped ->
        match parseFields stripped, commented with
        | Ok fields, false -> Case (number, fields)
        | Ok fields, true -> CommentedCase (number, fields)
        | Error reason, _ -> Malformed (number, reason)

/// <summary>Reads and classifies every line of a .tests file.</summary>
/// <param name="path">The file.</param>
/// <returns>The lines, in order.</returns>
let parse (path: string) : TestLine list =
    File.ReadAllLines path |> Array.toList |> List.mapi (fun index raw -> parseLine (index + 1) raw)
