module Farfbuzz.Harness.ShapeRequest

open System
open System.Globalization
open Farfbuzz

/// <summary>Buffer flags hb-shape can set, the values of <c>hb_buffer_flags_t</c>.</summary>
[<Flags>]
type BufferFlags =
    | Default = 0u
    | BeginningOfText = 1u
    | EndOfText = 2u
    | PreserveDefaultIgnorables = 4u
    | RemoveDefaultIgnorables = 8u
    | DoNotInsertDottedCircle = 16u
    | Verify = 32u
    | ProduceUnsafeToConcat = 64u
    | ProduceSafeToInsertTatweel = 128u

/// <summary>Everything one hb-shape invocation does, typed. Feature, variation, direction, script and language stay
/// raw strings: each shaper parses them with its own library, so the oracle never depends on Farfbuzz's parsers.</summary>
type ShapeRequest =
    { FontPath: string
      FaceIndex: int
      FontSize: struct (double * double) voption  // none means the face's units per em
      Ptem: double
      Slant: double
      Embolden: struct (double * double)          // --font-bold, not in place
      Variations: string voption                  // the raw --variations argument
      Text: uint32 array                          // decoded --unicodes
      Before: uint32 array                        // decoded --unicodes-before
      After: uint32 array                         // decoded --unicodes-after
      Features: string voption                    // the raw --features argument
      Direction: string voption
      Script: string voption
      Language: string                            // "c" unless given: run-tests.py runs hb-shape with LC_ALL=C
      ClusterLevel: int
      Flags: BufferFlags
      NotFoundVariationSelectorGlyph: uint32      // HB_CODEPOINT_INVALID unless given
      SinglePar: bool
      Shaper: string voption                      // --shaper, as run-tests.py appends it
      FontFuncs: string voption                   // --font-funcs
      Output: SerializeFlags }

/// <summary><c>HB_CODEPOINT_INVALID</c>.</summary>
[<Literal>]
let InvalidCodepoint = 0xFFFFFFFFu

/// <summary>The bytes <c>encode_unicodes</c> skips between values (util/text-options.hh).</summary>
let unicodeDelimiters = "<+-|>{},;&#\\xXuUnNiI\n\t\v\f\r "

/// <summary>Reads hexadecimal digits from an index, with strtoul's optional 0x prefix.</summary>
/// <param name="text">The argument.</param>
/// <param name="index">The first character of the value.</param>
/// <returns>The value and the index after it, or none when no digit follows.</returns>
let hexValue (text: string) (index: int) : struct (uint64 * int) voption =
    let isHex (c: char) = Uri.IsHexDigit c
    let start =
        match index + 2 < text.Length && text[index] = '0' && (text[index + 1] = 'x' || text[index + 1] = 'X') && isHex text[index + 2] with
        | true -> index + 2
        | false -> index
    let rec digitsEnd at = if at < text.Length && isHex text[at] then digitsEnd (at + 1) else at
    match digitsEnd start with
    | stop when stop = start -> ValueNone
    | stop -> ValueSome (struct (UInt64.Parse(text.AsSpan(start, stop - start), NumberStyles.HexNumber, CultureInfo.InvariantCulture), stop))

/// <summary>Decodes a --unicodes argument as <c>encode_unicodes</c> does; a lone '*' is the character '*'.</summary>
/// <param name="text">The argument.</param>
/// <returns>The code points, or the position of the first invalid value.</returns>
let decodeUnicodes (text: string) : Result<uint32 array, string> =
    let rec decode (index: int) (decoded: uint32 list) =
        match index < text.Length with
        | false -> Ok (List.rev decoded |> List.toArray)
        | true when unicodeDelimiters.Contains text[index] -> decode (index + 1) decoded
        | true ->
            match hexValue text index with
            | ValueSome (struct (value, next)) -> decode next (uint32 value :: decoded)
            | ValueNone -> Error $"Failed parsing Unicode value at: '{text.Substring index}'"
    match text with
    | "*" -> Ok [| uint32 '*' |]
    | _ -> decode 0 []

/// <summary>Parses one or two numbers separated by spaces or commas, as hb-shape's <c>sscanf("%lf%*[ ,]%lf")</c>.</summary>
/// <param name="text">The argument.</param>
/// <returns>The x and y values (y = x when only one is given).</returns>
let numberPair (text: string) : Result<struct (double * double), string> =
    let parse (value: string) = Double.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture)
    match text.Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries) with
    | [| x |] ->
        match parse x with
        | true, value -> Ok (struct (value, value))
        | false, _ -> Error $"not a number: {text}"
    | [| x; y |] ->
        match parse x, parse y with
        | (true, first), (true, second) -> Ok (struct (first, second))
        | _ -> Error $"not two numbers: {text}"
    | _ -> Error $"one or two numbers expected: {text}"

/// <summary>Options that take a value, as hb-shape declares them.</summary>
let valued =
    set [ "--font-size"; "--font-ptem"; "--font-slant"; "--font-bold"; "--font-grade"; "--font-ppem"; "--variations"
          "--named-instance"; "--face-index"; "--unicodes"; "--unicodes-before"; "--unicodes-after"; "--features"
          "--direction"; "--script"; "--language"; "--cluster-level"; "--invisible-glyph"; "--not-found-glyph"
          "--not-found-variation-selector-glyph"; "--shaper"; "--shapers"; "--font-funcs"; "--face-loader"; "--text" ]

/// <summary>Splits options into (name, value) pairs, accepting <c>--name=value</c> and <c>--name value</c>.</summary>
/// <param name="options">The option words.</param>
/// <returns>The pairs, a flag having no value, or the first option hb-shape would reject.</returns>
let rec pairs (options: string list) : Result<(string * string voption) list, string> =
    match options with
    | [] -> Ok []
    | (option: string) :: rest when option.Contains '=' ->
        let at = option.IndexOf '='
        pairs rest |> Result.map (fun tail -> (option.Substring(0, at), ValueSome (option.Substring(at + 1))) :: tail)
    | option :: value :: rest when valued.Contains option -> pairs rest |> Result.map (fun tail -> (option, ValueSome value) :: tail)
    | option :: [] when valued.Contains option -> Error $"{option} needs a value"
    | option :: rest -> pairs rest |> Result.map (fun tail -> (option, ValueNone) :: tail)

/// <summary>The request of a test before its options are applied.</summary>
/// <param name="fontPath">The resolved font file.</param>
/// <param name="text">The decoded --unicodes.</param>
/// <returns>hb-shape's defaults.</returns>
let defaults (fontPath: string) (text: uint32 array) =
    { FontPath = fontPath; FaceIndex = 0; FontSize = ValueNone; Ptem = 0.0; Slant = 0.0; Embolden = struct (0.0, 0.0)
      Variations = ValueNone; Text = text; Before = [||]; After = [||]; Features = ValueNone; Direction = ValueNone
      Script = ValueNone; Language = "c"; ClusterLevel = 0; Flags = BufferFlags.Default
      NotFoundVariationSelectorGlyph = InvalidCodepoint; SinglePar = false; Shaper = ValueNone; FontFuncs = ValueNone
      Output = SerializeFlags.Default }

/// <summary>Applies one option to a request; an option the harness does not know is an error, never ignored.</summary>
/// <param name="request">The request so far.</param>
/// <param name="name">The option name.</param>
/// <param name="value">Its value, for options that take one.</param>
/// <returns>The updated request, or why the option cannot be applied.</returns>
let apply (request: ShapeRequest) (name: string) (value: string voption) : Result<ShapeRequest, string> =
    let flag (bits: BufferFlags) = Ok { request with Flags = request.Flags ||| bits }
    let output (bits: SerializeFlags) = Ok { request with Output = request.Output ||| bits }
    let number (text: string) = Double.Parse(text, NumberStyles.Float, CultureInfo.InvariantCulture)
    match name, value with
    | "--no-glyph-names", ValueNone -> output SerializeFlags.NoGlyphNames
    | "--no-positions", ValueNone -> output SerializeFlags.NoPositions
    | "--no-clusters", ValueNone -> output SerializeFlags.NoClusters
    | "--no-advances", ValueNone -> output SerializeFlags.NoAdvances
    | "--ned", ValueNone -> output (SerializeFlags.NoClusters ||| SerializeFlags.NoAdvances)
    | "--show-flags", ValueNone -> output SerializeFlags.GlyphFlags
    | "--show-extents", ValueNone -> output SerializeFlags.GlyphExtents
    | "--bot", ValueNone -> flag BufferFlags.BeginningOfText
    | "--eot", ValueNone -> flag BufferFlags.EndOfText
    | "--preserve-default-ignorables", ValueNone -> flag BufferFlags.PreserveDefaultIgnorables
    | "--remove-default-ignorables", ValueNone -> flag BufferFlags.RemoveDefaultIgnorables
    | "--verify", ValueNone -> flag BufferFlags.Verify
    | "--unsafe-to-concat", ValueNone -> flag BufferFlags.ProduceUnsafeToConcat
    | "--safe-to-insert-tatweel", ValueNone -> flag BufferFlags.ProduceSafeToInsertTatweel
    | "--single-par", ValueNone -> Ok { request with SinglePar = true }
    | "--font-size", ValueSome "upem" -> Ok { request with FontSize = ValueNone }
    | "--font-size", ValueSome size -> numberPair size |> Result.map (fun pair -> { request with FontSize = ValueSome pair })
    | "--font-ptem", ValueSome ptem -> Ok { request with Ptem = number ptem }
    | "--font-slant", ValueSome slant -> Ok { request with Slant = number slant }
    | "--font-bold", ValueSome bold -> numberPair bold |> Result.map (fun pair -> { request with Embolden = pair })
    | "--face-index", ValueSome index -> Ok { request with FaceIndex = int index }
    | "--variations", ValueSome variations -> Ok { request with Variations = ValueSome variations }
    | "--features", ValueSome features -> Ok { request with Features = ValueSome features }
    | "--direction", ValueSome direction -> Ok { request with Direction = ValueSome direction }
    | "--script", ValueSome script -> Ok { request with Script = ValueSome script }
    | "--language", ValueSome language -> Ok { request with Language = language }
    | "--cluster-level", ValueSome level -> Ok { request with ClusterLevel = int level }
    | "--not-found-variation-selector-glyph", ValueSome glyph -> Ok { request with NotFoundVariationSelectorGlyph = uint32 glyph }
    | "--unicodes-before", ValueSome text -> decodeUnicodes text |> Result.map (fun decoded -> { request with Before = decoded })
    | "--unicodes-after", ValueSome text -> decodeUnicodes text |> Result.map (fun decoded -> { request with After = decoded })
    | ("--shaper" | "--shapers"), ValueSome shaper -> Ok { request with Shaper = ValueSome shaper }
    | "--font-funcs", ValueSome funcs -> Ok { request with FontFuncs = ValueSome funcs }
    | "--face-loader", ValueSome _ -> Ok request
    | other, _ -> Error $"option not modelled by the harness: {other}"

/// <summary>Builds the request for one run: the test's options, then the options run-tests.py appends.</summary>
/// <param name="fontPath">The resolved font file.</param>
/// <param name="unicodes">The --unicodes field.</param>
/// <param name="options">The test's own options, backend pins removed.</param>
/// <param name="extra">The options run-tests.py appends (backends and verification).</param>
/// <returns>The request, or why hb-shape would reject the invocation.</returns>
let build (fontPath: string) (unicodes: string) (options: string list) (extra: string list) : Result<ShapeRequest, string> =
    match decodeUnicodes unicodes, pairs (options @ extra) with
    | Ok text, Ok named -> named |> List.fold (fun request (name, value) -> request |> Result.bind (fun current -> apply current name value)) (Ok (defaults fontPath text))
    | Error reason, _ -> Error reason
    | _, Error reason -> Error reason
