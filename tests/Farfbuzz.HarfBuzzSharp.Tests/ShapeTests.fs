module Farfbuzz.HarfBuzzSharp.Tests.ShapeTests

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Threading.Tasks
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Diff
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>Why a request is not run through the facade: it asks for something the facade's public API cannot express.</summary>
/// <param name="request">The request.</param>
/// <returns>The reason, or none when the facade can run it.</returns>
let unsupported (request: ShapeRequest) =
    match request with
    | _ when request.Ptem <> 0.0 -> ValueSome "ptem"
    | _ when request.Slant <> 0.0 -> ValueSome "slant"
    | _ when request.Embolden <> struct (0.0, 0.0) -> ValueSome "embolden"
    | _ when request.NotFoundVariationSelectorGlyph <> InvalidCodepoint -> ValueSome "not-found variation selector glyph"
    | _ when ValueOption.isSome request.FontFuncs && request.FontFuncs <> ValueSome "ot" -> ValueSome "font funcs"
    | _ -> ValueNone

/// <summary>Maps the core's direction to the facade's.</summary>
/// <param name="direction">The core direction.</param>
/// <returns>The facade direction.</returns>
let private facadeDirection (direction: Direction) =
    enum<HarfBuzzSharp.Direction> (int (Direction.value direction))

/// <summary>Shapes a request through the facade the way Avalonia drives it: buffer text in three adds, segment properties, guess, shape.</summary>
/// <param name="faceOf">Makes the face of a file and index.</param>
/// <param name="request">The request.</param>
/// <returns>The glyph infos and positions, converted to the core's structs.</returns>
let rec shapeThroughFacade (faceOf: string -> int -> HarfBuzzSharp.Face) (request: ShapeRequest) =
    let face = faceOf request.FontPath request.FaceIndex
    use font = new HarfBuzzSharp.Font(face)
    shapeUsing face font request

/// <summary>Shapes a request with a font the caller made, after setting its scale from the request.</summary>
/// <param name="face">The font's face.</param>
/// <param name="font">The font, whose scale is set here.</param>
/// <param name="request">The request.</param>
/// <returns>The glyph infos and positions, converted to the core's structs.</returns>
and shapeUsing (face: HarfBuzzSharp.Face) (font: HarfBuzzSharp.Font) (request: ShapeRequest) =
    shapeUsingUnicode HarfBuzzSharp.UnicodeFunctions.Default face font request

/// <summary>Shapes a request with a font the caller made and the Unicode functions the caller chose for the buffer.</summary>
/// <param name="unicode">The Unicode functions of the buffer.</param>
/// <param name="face">The font's face.</param>
/// <param name="font">The font, whose scale is set here.</param>
/// <param name="request">The request.</param>
/// <returns>The glyph infos and positions, converted to the core's structs.</returns>
and shapeUsingUnicode (unicode: HarfBuzzSharp.UnicodeFunctions) (face: HarfBuzzSharp.Face) (font: HarfBuzzSharp.Font) (request: ShapeRequest) =
    let upem = float face.UnitsPerEm
    let struct (sizeX, sizeY) = match request.FontSize with | ValueSome size -> size | ValueNone -> struct (upem, upem)
    font.SetScale(int (float32 sizeX), int (float32 sizeY))
    match request.Variations with
    | ValueSome raw ->
        let variations =
            raw.Split([| ','; ' ' |])
            |> Array.choose (fun item ->
                match Farfbuzz.Variation.parse item with
                | Ok variation -> Some (HarfBuzzSharp.Variation(Tag = uint32 variation.Tag, Value = variation.Value))
                | Error _ -> None)
        font.SetVariations(ReadOnlySpan variations)
    | ValueNone -> ()
    use buffer = new HarfBuzzSharp.Buffer()
    buffer.UnicodeFunctions <- unicode
    buffer.ClusterLevel <- enum<HarfBuzzSharp.ClusterLevel> request.ClusterLevel
    buffer.Flags <- enum<HarfBuzzSharp.BufferFlags> (int (uint32 request.Flags))
    match request.Before.Length > 0 with
    | true -> buffer.AddCodepoints(ReadOnlySpan<uint32> request.Before, request.Before.Length, 0)
    | false -> ()
    buffer.AddCodepoints(ReadOnlySpan<uint32> request.Text, 0, request.Text.Length)
    match request.After.Length > 0 with
    | true -> buffer.AddCodepoints(ReadOnlySpan<uint32> request.After, 0, 0)
    | false -> ()
    match request.Direction with
    | ValueSome text -> buffer.Direction <- facadeDirection (Direction.ofString text)
    | ValueNone -> ()
    match request.Script with
    | ValueSome text -> buffer.Script <- HarfBuzzSharp.Script.Parse text
    | ValueNone -> ()
    use language = new HarfBuzzSharp.Language(request.Language)
    buffer.Language <- language
    buffer.GuessSegmentProperties()
    let features =
        match request.Features with
        | ValueNone -> [||]
        | ValueSome raw ->
            let unquoted =
                match raw.Length >= 1 && raw[0] = raw[raw.Length - 1] && (raw[0] = '"' || raw[0] = '\'') with
                | true -> raw.Substring(1, max 0 (raw.Length - 2))
                | false -> raw
            match unquoted.Length with
            | 0 -> [||]
            | _ ->
                unquoted.Split([| ','; ' ' |])
                |> Array.choose (fun item -> match HarfBuzzSharp.Feature.TryParse item with | true, feature -> Some feature | false, _ -> None)
    let shapers = match request.Shaper with | ValueSome text -> text.Split ',' | ValueNone -> Unchecked.defaultof<string array>
    font.Shape(buffer, features, shapers)
    let infos = buffer.GlyphInfos
    let positions = buffer.GlyphPositions
    MemoryMarshal.Cast<HarfBuzzSharp.GlyphInfo, GlyphInfo>(ReadOnlySpan<HarfBuzzSharp.GlyphInfo> infos).ToArray(),
    MemoryMarshal.Cast<HarfBuzzSharp.GlyphPosition, GlyphPosition>(ReadOnlySpan<HarfBuzzSharp.GlyphPosition> positions).ToArray()

/// <summary>Every planned run of the three shaping suites the core engine can run, as requests.</summary>
/// <returns>The names and requests.</returns>
let suiteRequests () =
    Suites.registered (RepositoryPaths.upstream ())
    |> List.collect (fun suite -> suite.Files)
    |> List.collect (fun entry -> Runner.plan FarfbuzzShaper.shaper entry)
    |> List.choose (fun spec ->
        match spec.Kind with
        | Runner.Execute (fontPath, unicodes, options, extra, _) ->
            match ShapeRequest.build fontPath unicodes options extra with
            | Ok request when not request.SinglePar && Array.contains 0x0Au request.Text -> None
            | Ok request -> Some (Runner.displayName spec.Id, request)
            | Error _ -> None
        | Runner.Skipped _ | Runner.Unrunnable _ -> None)

/// <summary>The P10.S08 cases: fonts, scale, shaping through the facade.</summary>
[<Tests>]
let tests =
    testList "P10.S08" [
        testCase "a line that shapes into more glyphs than the buffer holds grows the buffer and equals the core's glyphs" (fun () ->
            let path = System.IO.Path.Combine(RepositoryPaths.upstream (), "test", "subset", "data", "fonts", "Roboto-Regular.ttf")
            let face = FacadeFonts.faceOf path 0
            use font = new HarfBuzzSharp.Font(face)
            let coreFont = Font.create (Face.create (ReadOnlyMemory (FacadeFonts.bytesOf path)) 0u)
            let shapeGrown (codepoint: uint32) =
                let text = Array.create 12 codepoint
                use buffer = new HarfBuzzSharp.Buffer()
                buffer.AddCodepoints(ReadOnlySpan<uint32> text, 0, text.Length)
                buffer.GuessSegmentProperties()
                font.Shape buffer
                let infos = buffer.GlyphInfos
                let positions = buffer.GlyphPositions
                let run = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addCodepoints (ReadOnlySpan<uint32> text) 0 text.Length GlyphRun.empty |> TailCalls.settle)
                let struct (shaped, _) = Shape.shapeCached PlanCache.empty coreFont ImmutableArray<Feature>.Empty ValueNone run
                match shaped with
                | Error _ -> struct (false, false)
                | Ok expected ->
                    let same =
                        MemoryMarshal.Cast<HarfBuzzSharp.GlyphInfo, GlyphInfo>(ReadOnlySpan<HarfBuzzSharp.GlyphInfo> infos).ToArray() = expected.Infos.AsSpan().ToArray()
                        && MemoryMarshal.Cast<HarfBuzzSharp.GlyphPosition, GlyphPosition>(ReadOnlySpan<HarfBuzzSharp.GlyphPosition> positions).ToArray() = expected.Positions.AsSpan().ToArray()
                    struct (infos.Length > text.Length, same)
            let candidates = [| 0x01D7u; 0x01D8u; 0x01DBu; 0x01DCu; 0x0209u; 0x1E08u |]
            let outcomes = candidates |> Array.map shapeGrown
            Expect.isTrue (outcomes |> Array.exists (fun (struct (grown, _)) -> grown)) "some Latin letter the face lacks decomposes, so the line has more glyphs than the buffer's capacity"
            Expect.isTrue (outcomes |> Array.forall (fun (struct (_, same)) -> same)) "every line equals the core's glyphs and positions")
        testCase "every suite run the facade can express shapes through the facade as it does through the core, glyph for glyph" (fun () ->
            let requests = suiteRequests ()
            Expect.isGreaterThan requests.Length 5000 "the suites"
            let outcomes =
                requests
                |> List.map (fun (name, request) ->
                    match unsupported request with
                    | ValueSome reason -> name, Choice1Of4 reason
                    | ValueNone ->
                        match FarfbuzzShaper.shaper.Shape request with
                        | Error reason -> name, Choice2Of4 reason
                        | Ok ShapingFailed -> name, Choice2Of4 "shaping failed"
                        | Ok (Shaped core) ->
                            let facade = try Ok (shapeThroughFacade FacadeFonts.faceOf request) with error -> Error $"{error.GetType().Name}: {error.Message}"
                            match facade with
                            | Error reason -> name, Choice3Of4 reason
                            | Ok (infos, positions) ->
                                match infos = core.Infos && positions = core.Positions with
                                | true -> name, Choice4Of4 ()
                                | false -> name, Choice3Of4 $"glyphs differ: core {core.Infos.Length} glyphs, facade {infos.Length}")
            let count pick = outcomes |> List.filter (snd >> pick) |> List.length
            let skippedByFacade = count (function | Choice1Of4 _ -> true | _ -> false)
            let skippedByCore = count (function | Choice2Of4 _ -> true | _ -> false)
            let agreed = count (function | Choice4Of4 _ -> true | _ -> false)
            let differing = outcomes |> List.choose (fun (name, outcome) -> match outcome with | Choice3Of4 reason -> Some $"{name}: {reason}" | _ -> None)
            printfn $"P10.S08: {requests.Length} suite runs; {agreed} agree glyph for glyph; {skippedByFacade} use options the facade cannot express (ptem, slant, embolden, not-found variation selector glyph, font funcs); {skippedByCore} the core engine itself reports as not run; {differing.Length} differ"
            Expect.isGreaterThan agreed 4000 "the facade agrees on most of the suites"
            Expect.isEmpty (List.truncate 10 differing) "runs on which the facade and the core differ")
        testCase "a face made from a table delegate shapes every suite run as the face made from the file does" (fun () ->
            let requests = suiteRequests () |> List.filter (fun (_, request) -> ValueOption.isNone (unsupported request))
            let mismatches =
                requests
                |> List.choose (fun (name, request) ->
                    let fromFile = try Ok (shapeThroughFacade FacadeFonts.faceOf request) with error -> Error error.Message
                    let fromTables = try Ok (shapeThroughFacade FacadeFonts.callbackFaceOf request) with error -> Error error.Message
                    match fromFile = fromTables with
                    | true -> None
                    | false -> Some name)
            printfn $"P10.S08: {requests.Length} runs through a table-delegate face; {mismatches.Length} differ from the file face"
            Expect.isEmpty (List.truncate 40 mismatches) ("runs on which the two faces differ:\n" + String.concat "\n" (List.truncate 40 mismatches)))
        test "Shape throws on an invalid direction or a non-Unicode buffer, before it looks at the length, and leaves an empty buffer alone" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
            use buffer = new HarfBuzzSharp.Buffer()
            let direction =
                try
                    font.Shape(buffer)
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal direction (Some "Buffer's Direction must be valid.") "invalid direction first"
            buffer.Direction <- HarfBuzzSharp.Direction.LeftToRight
            let content =
                try
                    font.Shape(buffer)
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal content (Some "Buffer's ContentType must of type Unicode.") "content type"
            buffer.ContentType <- HarfBuzzSharp.ContentType.Unicode
            font.Shape(buffer)
            Expect.equal (buffer.Length, buffer.ContentType) (0, HarfBuzzSharp.ContentType.Unicode) "an empty buffer is returned at once with its content type unchanged"
            Expect.throwsT<ArgumentNullException> (fun () -> font.Shape(Unchecked.defaultof<HarfBuzzSharp.Buffer>)) "null buffer"
        }
        test "scale, sub-fonts and the shaper list" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            let face = FacadeFonts.faceOf path 0
            use font = new HarfBuzzSharp.Font(face)
            let mutable x = 0
            let mutable y = 0
            font.GetScale(&x, &y)
            Expect.equal (x, y) (face.UnitsPerEm, face.UnitsPerEm) "scale = upem at creation"
            font.SetScale(512, 640)
            font.GetScale(&x, &y)
            Expect.equal (x, y) (512, 640) "set scale"
            use sub = new HarfBuzzSharp.Font(font)
            sub.GetScale(&x, &y)
            Expect.equal (x, y) (512, 640) "a sub-font starts with its parent's scale"
            Expect.isTrue (obj.ReferenceEquals(sub.Parent, font)) "Parent"
            Expect.equal font.SupportedShapers [| "ot"; "fallback" |] "shapers"
            Expect.isNull font.Parent "a font made from a face has no parent"
        }
        test "a font keeps working after its face and blob are disposed (the SKShaper pattern)" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            let blob = FacadeFonts.blobOf path
            let face = new HarfBuzzSharp.Face(blob, 0)
            use font = new HarfBuzzSharp.Font(face)
            face.Dispose()
            blob.Dispose()
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf8 "Hello"
            buffer.GuessSegmentProperties()
            font.Shape buffer
            Expect.equal buffer.Length 5 "five glyphs"
            Expect.equal buffer.ContentType HarfBuzzSharp.ContentType.Glyphs "glyphs"
            Expect.isTrue (buffer.GlyphPositions |> Array.forall (fun position -> position.XAdvance > 0)) "advances"
        }
        test "setting the units per em of a face after a font was made from it is ignored" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use blob = FacadeFonts.blobOf path
            use face = new HarfBuzzSharp.Face(blob, 0)
            let own = face.UnitsPerEm
            use _font = new HarfBuzzSharp.Font(face)
            face.UnitsPerEm <- own * 2
            Expect.equal face.UnitsPerEm own "ignored"
            Expect.isTrue face.IsImmutable "frozen by the font"
        }
        testCase "eight tasks shaping with one shared font and a buffer each produce the sequential results" (fun () ->
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            let font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
            let lines = [| for index in 0 .. 63 -> "Line " + string index + " of the offices, fluffy waffles: 12.5% off" |]
            let shapeLine (line: string) =
                use buffer = new HarfBuzzSharp.Buffer()
                buffer.AddUtf16 line
                buffer.GuessSegmentProperties()
                font.Shape buffer
                buffer.GlyphInfos |> Array.map (fun info -> info.Codepoint, info.Cluster), buffer.GlyphPositions |> Array.map (fun position -> position.XAdvance, position.XOffset, position.YOffset)
            let sequential = lines |> Array.map shapeLine
            let concurrent = ConcurrentDictionary<int, (uint32 * uint32) array * (int * int * int) array>()
            Parallel.For(0, 8 * lines.Length, ParallelOptions(MaxDegreeOfParallelism = 8), fun index -> concurrent[index] <- shapeLine lines[index % lines.Length]) |> ignore
            let wrong = [ for index in 0 .. 8 * lines.Length - 1 do if concurrent[index] <> sequential[index % lines.Length] then yield index ]
            font.Dispose()
            Expect.isEmpty wrong "tasks whose result differs from the sequential one")
    ]
