module Farfbuzz.Harness.Runner

open System
open System.IO
open System.Security.Cryptography
open System.Text
open Farfbuzz
open Farfbuzz.Harness.Suites
open Farfbuzz.Harness.TestFile
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

/// <summary>The backends a run uses; none means run-tests.py appends no option for it.</summary>
type Backend = { Shaper: string voption; FaceLoader: string voption; FontFuncs: string voption }

/// <summary>Identifies one run: display fields, plus a key that survives upstream line shifts.</summary>
type TestId =
    { Suite: string
      File: string
      Registration: int
      Line: int
      Backend: Backend
      Key: string }  // 12 hex digits of SHA-1 over the normalised test content and backend

/// <summary>Why a test produced no shaping run.</summary>
type SkipReason =
    | BackendNotProvided of kind: string * backend: string  // run-tests.py prints "ok N # skip"
    | SystemFontMissing of path: string                     // counted as skipped, no TAP line
    | SystemFontVersion of path: string * expected: string * actual: string

/// <summary>What one planned run will do.</summary>
type RunKind =
    | Execute of fontPath: string * unicodes: string * options: string list * extra: string list * expected: Expected
    | Skipped of reason: SkipReason
    | Unrunnable of reason: string  // a line run-tests.py itself would crash on

/// <summary>One planned run of one test line.</summary>
type RunSpec = { Id: TestId; Kind: RunKind }

/// <summary>The result of one run.</summary>
type Outcome =
    | Pass
    | Fail of expected: string * actual: string
    | Skip of reason: SkipReason
    | Broken of reason: string  // the harness or the engine could not run it

/// <summary>Spells a run for test names and reports, for example <c>in-house/arabic-fallback-shaping.tests#3:12 [ot/ot]</c>.</summary>
/// <param name="id">The run identity.</param>
/// <returns>The name.</returns>
let displayName (id: TestId) =
    let spell (value: string voption) = match value with ValueSome text -> text | ValueNone -> "-"
    $"{id.Suite}/{id.File}#{id.Registration}:{id.Line} [{spell id.Backend.Shaper}/{spell id.Backend.FontFuncs}]"

/// <summary>Derives the stable key of a run from its content.</summary>
/// <param name="entry">The test file.</param>
/// <param name="fields">The test fields.</param>
/// <param name="backend">The backends.</param>
/// <returns>12 hex digits.</returns>
let key (entry: TestFileEntry) (fields: CaseFields) (backend: Backend) =
    let spell (value: string voption) = match value with ValueSome text -> text | ValueNone -> String.Empty
    let font = match fields.Font with Relative path -> path | Absolute (path, _) -> path
    let content =
        String.Join("|", [ entry.Suite; entry.Name; string entry.Registration; font; String.Join(" ", fields.Options); fields.Unicodes
                           spell backend.Shaper; spell backend.FaceLoader; spell backend.FontFuncs ])
    Convert.ToHexString(SHA1.HashData(Encoding.UTF8.GetBytes content)).Substring(0, 12).ToLowerInvariant()

/// <summary>The backend lists run-tests.py iterates over, as directives leave them.</summary>
type Selection = { Shapers: string list; FaceLoaders: string list; FontFuncs: string list }

/// <summary>The selection at the start of every file: shaper ot, face loader ot, every supported font-funcs.</summary>
/// <param name="shaper">The engine.</param>
/// <returns>The initial selection.</returns>
let initialSelection (shaper: Shaper) = { Shapers = [ "ot" ]; FaceLoaders = [ "ot" ]; FontFuncs = shaper.FontFuncs }

/// <summary>The backends an engine supports for a kind.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="kind">The kind.</param>
/// <returns>The supported names.</returns>
let supported (shaper: Shaper) (kind: BackendKind) =
    match kind with
    | BackendKind.Shaper -> shaper.Shapers
    | FaceLoader -> shaper.FaceLoaders
    | FontFuncs -> shaper.FontFuncs

/// <summary>Applies a directive: keep the listed supported values, or every supported value not listed.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="selection">The current selection.</param>
/// <param name="directive">The directive.</param>
/// <returns>The new selection.</returns>
let applyDirective (shaper: Shaper) (selection: Selection) (directive: Directive) =
    let available = supported shaper directive.Kind
    let chosen =
        match directive.Exclude with
        | true -> available |> List.filter (fun value -> not (List.contains value directive.Values))
        | false -> directive.Values |> List.filter (fun value -> List.contains value available)
    match directive.Kind with
    | BackendKind.Shaper -> { selection with Shapers = chosen }
    | FaceLoader -> { selection with FaceLoaders = chosen }
    | FontFuncs -> { selection with FontFuncs = chosen }

/// <summary>Removes backend pins from a test's options as run-tests.py does, stopping at the first unsupported one.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="options">The option words.</param>
/// <returns>The remaining options and the pins, or the unsupported pin.</returns>
let rec stripPins (shaper: Shaper) (options: string list) : Result<string list * Backend, BackendKind * string> =
    let kindOf (option: string) = [ BackendKind.Shaper; FaceLoader; FontFuncs ] |> List.tryFind (fun kind -> option.StartsWith("--" + kindName kind))
    let pin (kind: BackendKind) (value: string) (rest: string list) =
        match List.contains value (supported shaper kind), stripPins shaper rest with
        | false, _ -> Error (kind, value)
        | true, Error unsupported -> Error unsupported
        | true, Ok (remaining, backend) ->
            match kind with
            | BackendKind.Shaper -> Ok (remaining, { backend with Shaper = ValueSome value })
            | FaceLoader -> Ok (remaining, { backend with FaceLoader = ValueSome value })
            | FontFuncs -> Ok (remaining, { backend with FontFuncs = ValueSome value })
    match options with
    | [] -> Ok ([], { Shaper = ValueNone; FaceLoader = ValueNone; FontFuncs = ValueNone })
    | option :: rest ->
        match kindOf option with
        | Some kind when option.Contains '=' -> pin kind (option.Split('=')[1]) rest
        | Some kind ->
            match rest with
            | value :: afterValue -> pin kind value afterValue
            | [] -> Error (kind, String.Empty)
        | None -> stripPins shaper rest |> Result.map (fun (remaining, backend) -> option :: remaining, backend)

/// <summary>
/// Expands the backend loops of run-tests.py, including its quirk that the loop variables are globals: an inner
/// loop that finished leaves its last value behind, and that value pins the next iteration of the outer loop.
/// </summary>
/// <param name="selection">The selection in effect.</param>
/// <param name="pinned">The backends the line pins.</param>
/// <returns>The backends of each run, in order.</returns>
let combinations (selection: Selection) (pinned: Backend) : Backend list =
    let orAll (value: string voption) (all: string list) = match value with ValueSome one -> [ ValueSome one ] | ValueNone -> all |> List.map ValueSome
    let faceLoaders = orAll pinned.FaceLoader selection.FaceLoaders
    let step (shaperGlobal: string voption, fontFuncsGlobal: string voption, runs: Backend list) (faceLoader: string voption) =
        let shapers = orAll shaperGlobal selection.Shapers
        shapers
        |> List.fold (fun (_, funcsGlobal, acc) shaper ->
            let fontFuncsList =
                match funcsGlobal with
                | ValueSome funcs -> [ ValueSome funcs ]
                | ValueNone -> if shaper = ValueSome "ot" then selection.FontFuncs |> List.map ValueSome else [ ValueNone ]
            let lastFuncs = match List.tryLast fontFuncsList with Some funcs -> funcs | None -> funcsGlobal
            let runs = fontFuncsList |> List.map (fun funcs -> { Shaper = shaper; FaceLoader = faceLoader; FontFuncs = funcs })
            (shaper, lastFuncs, acc @ runs)) (shaperGlobal, fontFuncsGlobal, runs)
    let _, _, runs = faceLoaders |> List.fold step (pinned.Shaper, pinned.FontFuncs, [])
    runs

/// <summary>The options run-tests.py appends to one run.</summary>
/// <param name="backend">The run's backends.</param>
/// <param name="expected">The expectation; verification options are added unless it is '*'.</param>
/// <returns>The extra option words.</returns>
let extraOptions (backend: Backend) (expected: Expected) =
    let named (prefix: string) (value: string voption) = match value with ValueSome text -> [ prefix + text ] | ValueNone -> []
    let verification = match expected with Any -> [] | Glyphs _ -> [ "--verify"; "--unsafe-to-concat"; "--safe-to-insert-tatweel" ]
    named "--shaper=" backend.Shaper @ named "--face-loader=" backend.FaceLoader @ named "--font-funcs=" backend.FontFuncs @ verification

/// <summary>Resolves a test's font as run-tests.py does, checking system fonts against their SHA-1.</summary>
/// <param name="entry">The test file.</param>
/// <param name="font">The font field.</param>
/// <returns>The path, or why the test is skipped.</returns>
let resolveFont (entry: TestFileEntry) (font: FontRef) : Result<string, SkipReason> =
    match font with
    | Relative path -> Ok (Path.GetFullPath(Path.Combine(Path.GetDirectoryName entry.Path |> nonNull, path)))
    | Absolute (path, _) when not (File.Exists path) -> Error (SystemFontMissing path)
    | Absolute (path, ValueNone) -> Ok path
    | Absolute (path, ValueSome expected) ->
        match Convert.ToHexString(SHA1.HashData(File.ReadAllBytes path)).ToLowerInvariant() with
        | actual when actual = expected -> Ok path
        | actual -> Error (SystemFontVersion (path, expected, actual))

/// <summary>
/// Makes keys unique within a file: a test line repeated verbatim (as in macos.tests or SHKNDA-1.tests) gets its
/// occurrence number appended, so keys stay independent of line numbers yet never collide.
/// </summary>
/// <param name="runs">The planned runs of one file, in order.</param>
/// <returns>The runs with unique keys.</returns>
let distinguishRepeats (runs: RunSpec list) =
    let folder (seen: Map<string, int>, renamed: RunSpec list) (run: RunSpec) =
        match Map.tryFind run.Id.Key seen with
        | None -> (Map.add run.Id.Key 1 seen, run :: renamed)
        | Some count -> (Map.add run.Id.Key (count + 1) seen, { run with Id = { run.Id with Key = $"{run.Id.Key}-{count + 1}" } } :: renamed)
    runs |> List.fold folder (Map.empty, []) |> snd |> List.rev

/// <summary>Plans the runs of one test file without shaping anything.</summary>
/// <param name="shaper">The engine, for its supported backends.</param>
/// <param name="entry">The test file.</param>
/// <returns>The runs, in run-tests.py order.</returns>
let plan (shaper: Shaper) (entry: TestFileEntry) : RunSpec list =
    let noBackend = { Shaper = ValueNone; FaceLoader = ValueNone; FontFuncs = ValueNone }
    let identify line fields backend = { Suite = entry.Suite; File = entry.Name; Registration = entry.Registration; Line = line; Backend = backend; Key = key entry fields backend }
    let planCase (selection: Selection) (line: int) (fields: CaseFields) (commented: bool) =
        match resolveFont entry fields.Font, commented with
        | Error reason, _ -> [ { Id = identify line fields noBackend; Kind = Skipped reason } ]
        | Ok _, true -> []
        | Ok fontPath, false ->
            match stripPins shaper fields.Options with
            | Error (kind, backend) -> [ { Id = identify line fields noBackend; Kind = Skipped (BackendNotProvided (kindName kind, backend)) } ]
            | Ok (options, pinned) ->
                combinations selection pinned
                |> List.map (fun backend ->
                    { Id = identify line fields backend
                      Kind = Execute (fontPath, fields.Unicodes, options, extraOptions backend fields.Expected, fields.Expected) })
    let folder (selection: Selection, runs: RunSpec list) (line: TestLine) =
        match line with
        | Directive (_, directive) -> (applyDirective shaper selection directive, runs)
        | Case (number, fields) -> (selection, runs @ planCase selection number fields false)
        | CommentedCase (number, fields) -> (selection, runs @ planCase selection number fields true)
        | Malformed (number, reason) ->
            let fields = { Font = Relative String.Empty; Options = []; Unicodes = String.Empty; Expected = Any }
            (selection, runs @ [ { Id = identify number fields noBackend; Kind = Unrunnable reason } ])
        | Ignored _ -> (selection, runs)
    let runs = TestFile.parse entry.Path |> List.fold folder (initialSelection shaper, []) |> snd
    distinguishRepeats runs

/// <summary>Prints a shaping outcome as hb-shape does.</summary>
/// <param name="request">The request, for its output flags.</param>
/// <param name="outcome">The outcome.</param>
/// <returns>The output line.</returns>
let render (request: ShapeRequest) (outcome: ShapeOutcome) =
    match outcome with
    | Shaped run -> Serialize.glyphs request.Output run.GlyphName run.Extents (ReadOnlySpan run.Infos) (ReadOnlySpan run.Positions) |> TailCalls.settle
    | ShapingFailed -> ShapingFailedLine

/// <summary>
/// Re-serialises a glyph string by glyph id, as run-tests.py's second hb-shape pass (<c>--glyphs --no-glyph-names</c>)
/// does when an output mentions <c>gid</c>; an unreadable string gives the empty string, which always fails.
/// </summary>
/// <param name="run">The shaped run, for the font's name and character lookups.</param>
/// <param name="text">The glyph string.</param>
/// <returns>The string with glyph ids instead of names.</returns>
let normaliseGids (run: ShapedRun) (text: string) =
    match Serialize.parseGlyphs run.GlyphFromName run.NominalGlyph text with
    | Ok (struct (infos, positions)) ->
        Serialize.glyphs SerializeFlags.NoGlyphNames (fun _ -> ValueNone) (fun _ -> ValueNone) (infos.AsSpan()) (positions.AsSpan()) |> TailCalls.settle
    | Error _ -> String.Empty

/// <summary>Executes one planned run with an engine and compares its output with the expectation.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="spec">The planned run.</param>
/// <returns>The outcome.</returns>
let execute (shaper: Shaper) (spec: RunSpec) : Outcome =
    match spec.Kind with
    | Skipped reason -> Skip reason
    | Unrunnable reason -> Broken reason
    | Execute (fontPath, unicodes, options, extra, expected) ->
        match ShapeRequest.build fontPath unicodes options extra with
        | Error reason -> Broken reason
        | Ok request when not request.SinglePar && Array.contains 0x0Au request.Text -> Broken "several paragraphs: hb-shape would print one line each"
        | Ok request ->
            match shaper.Shape request, expected with
            | Error reason, _ -> Broken reason
            | Ok _, Any -> Pass
            | Ok outcome, Glyphs wanted ->
                let actual = render request outcome
                match outcome with
                | _ when actual = wanted -> Pass
                | Shaped run when actual.Contains "gid" ->
                    let normalisedWanted = normaliseGids run wanted
                    if normalisedWanted.Length > 0 && normaliseGids run actual = normalisedWanted then Pass else Fail (wanted, actual)
                | Shaped _ | ShapingFailed -> Fail (wanted, actual)
