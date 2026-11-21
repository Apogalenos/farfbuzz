module Farfbuzz.HarfBuzzSharp.Tests.UnicodeTests

open System
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>The functions of a child of the default ones whose every delegate asks the default functions.</summary>
/// <returns>The functions.</returns>
let private forwarding () =
    let defaults = HarfBuzzSharp.UnicodeFunctions.Default
    let functions = new HarfBuzzSharp.UnicodeFunctions(defaults)
    functions.SetCombiningClassDelegate(HarfBuzzSharp.CombiningClassDelegate(fun _ unicode -> defaults.GetCombiningClass unicode))
    functions.SetGeneralCategoryDelegate(HarfBuzzSharp.GeneralCategoryDelegate(fun _ unicode -> defaults.GetGeneralCategory unicode))
    functions.SetMirroringDelegate(HarfBuzzSharp.MirroringDelegate(fun _ unicode -> defaults.GetMirroring unicode))
    functions.SetScriptDelegate(HarfBuzzSharp.ScriptDelegate(fun _ unicode -> defaults.GetScript unicode))
    functions.SetComposeDelegate(HarfBuzzSharp.ComposeDelegate(fun _ a b ab -> let found, value = defaults.TryCompose(a, b) in ab <- value; found))
    functions.SetDecomposeDelegate(HarfBuzzSharp.DecomposeDelegate(fun _ ab a b -> let found, first, second = defaults.TryDecompose ab in a <- first; b <- second; found))
    functions

/// <summary>Shapes text through the facade with Roboto and the given Unicode functions, left to right Latin unless a direction is given.</summary>
/// <param name="unicode">The Unicode functions of the buffer.</param>
/// <param name="direction">The direction.</param>
/// <param name="text">The text.</param>
/// <returns>The glyph ids.</returns>
let private shaped (unicode: HarfBuzzSharp.UnicodeFunctions) (direction: HarfBuzzSharp.Direction) (text: string) =
    let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
    use buffer = new HarfBuzzSharp.Buffer()
    buffer.UnicodeFunctions <- unicode
    buffer.AddUtf16 text
    buffer.Direction <- direction
    buffer.Script <- HarfBuzzSharp.Script.Latin
    buffer.Language <- new HarfBuzzSharp.Language("en")
    font.Shape buffer
    buffer.GlyphInfos |> Array.map (fun info -> info.Codepoint)

/// <summary>The glyph a code point has in Roboto.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The glyph.</returns>
let private robotoGlyph (codepoint: uint32) =
    let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
    match font.TryGetNominalGlyph codepoint with
    | true, glyph -> glyph
    | false, _ -> failwith $"Roboto has no glyph for U+{codepoint:X4}"

/// <summary>The P10.S15 cases: replaceable Unicode functions.</summary>
[<Tests>]
let tests =
    testList "P10.S15" [
        testCase "the default functions answer as the oracle for every code point from U+0000 to U+10FFFF" (fun () ->
            OracleSession.ensure ()
            let defaults = HarfBuzzSharp.UnicodeFunctions.Default
            let differences = System.Collections.Generic.List<string>()
            let note (text: string) = if differences.Count < 10 then differences.Add text
            for codepoint in 0u .. 0x10FFFFu do
                let category = defaults.GetGeneralCategory codepoint
                let expectedCategory = Native.farfbuzz_oracle_general_category codepoint
                if uint32 category <> expectedCategory then note $"category U+{codepoint:X4}: {category} against {expectedCategory}"
                let combining = defaults.GetCombiningClass codepoint
                let expectedCombining = Native.farfbuzz_oracle_combining_class codepoint
                if uint32 combining <> expectedCombining then note $"combining class U+{codepoint:X4}: {combining} against {expectedCombining}"
                let mirroring = defaults.GetMirroring codepoint
                let expectedMirroring = Native.farfbuzz_oracle_mirroring codepoint
                if mirroring <> expectedMirroring then note $"mirroring U+{codepoint:X4}: {mirroring} against {expectedMirroring}"
                let script = uint32 (defaults.GetScript codepoint)
                let expectedScript = Native.farfbuzz_oracle_script codepoint
                if script <> expectedScript then note $"script U+{codepoint:X4}: {script:X8} against {expectedScript:X8}"
                let found, first, second = defaults.TryDecompose codepoint
                let mutable oracleFirst = 0u
                let mutable oracleSecond = 0u
                let oracleFound = Native.farfbuzz_oracle_decompose(codepoint, &oracleFirst, &oracleSecond) <> 0
                if found <> oracleFound || (found && (first, second) <> (oracleFirst, oracleSecond)) then note $"decompose U+{codepoint:X4}"
                if oracleFound then
                    let composed, ab = defaults.TryCompose(oracleFirst, oracleSecond)
                    let mutable oracleComposed = 0u
                    let oracleOk = Native.farfbuzz_oracle_compose(oracleFirst, oracleSecond, &oracleComposed) <> 0
                    if composed <> oracleOk || (composed && ab <> oracleComposed) then note $"compose U+{oracleFirst:X4} U+{oracleSecond:X4}"
                let pairComposed, pairAb = defaults.TryCompose(codepoint, 0x0301u)
                let mutable pairOracle = 0u
                let pairOk = Native.farfbuzz_oracle_compose(codepoint, 0x0301u, &pairOracle) <> 0
                if pairComposed <> pairOk || (pairComposed && pairAb <> pairOracle) then note $"compose U+{codepoint:X4} U+0301"
            Expect.isEmpty (List.ofSeq differences) (String.concat "\n" differences))
        testCase "functions whose delegates ask the default ones reproduce default shaping on 200 suite cases" (fun () ->
            let requests =
                ShapeTests.suiteRequests ()
                |> List.filter (fun (_, request) -> ValueOption.isNone (ShapeTests.unsupported request))
                |> List.indexed
                |> List.filter (fun (index, _) -> index % 27 = 0)
                |> List.map snd
                |> List.truncate 200
            Expect.isGreaterThan requests.Length 150 "cases"
            use functions = forwarding ()
            let differences =
                requests
                |> List.choose (fun (name, request) ->
                    let face = FacadeFonts.faceOf request.FontPath request.FaceIndex
                    use plain = new HarfBuzzSharp.Font(face)
                    let expected = ShapeTests.shapeUsing face plain request
                    use replaced = new HarfBuzzSharp.Font(face)
                    let actual = try Ok (ShapeTests.shapeUsingUnicode functions face replaced request) with error -> Error error.Message
                    match actual = Ok expected with
                    | true -> None
                    | false -> Some name)
            Expect.isEmpty differences (String.concat "\n" (List.truncate 10 differences)))
        test "a mirroring delegate decides which glyph a right-to-left character takes" {
            let plain = shaped HarfBuzzSharp.UnicodeFunctions.Default HarfBuzzSharp.Direction.RightToLeft "("
            Expect.equal plain [| robotoGlyph 0x29u |] "the default mirror of ( is )"
            use functions = new HarfBuzzSharp.UnicodeFunctions(HarfBuzzSharp.UnicodeFunctions.Default)
            functions.SetMirroringDelegate(HarfBuzzSharp.MirroringDelegate(fun _ unicode -> if unicode = 0x28u then 0x5Bu else unicode))
            Expect.equal (shaped functions HarfBuzzSharp.Direction.RightToLeft "(") [| robotoGlyph 0x5Bu |] "the delegate's mirror"
        }
        test "a compose delegate decides which character a pair becomes" {
            use functions = new HarfBuzzSharp.UnicodeFunctions(HarfBuzzSharp.UnicodeFunctions.Default)
            functions.SetComposeDelegate(HarfBuzzSharp.ComposeDelegate(fun _ a b ab ->
                match a = 0x61u && b = 0x0301u with
                | true -> ab <- 0x62u; true
                | false -> ab <- 0u; false))
            Expect.equal (shaped functions HarfBuzzSharp.Direction.LeftToRight "á") [| robotoGlyph 0x62u |] "a with acute becomes b"
            Expect.equal (shaped HarfBuzzSharp.UnicodeFunctions.Default HarfBuzzSharp.Direction.LeftToRight "a\u0301") [| robotoGlyph 0xE1u |] "the default composition is a with acute"
        }
        test "a combining-class delegate decides the order of marks" {
            let acute = robotoGlyph 0x0301u
            let below = robotoGlyph 0x0323u
            Expect.notEqual acute below "two glyphs"
            let plain = shaped HarfBuzzSharp.UnicodeFunctions.Default HarfBuzzSharp.Direction.LeftToRight "q̣́"
            Expect.equal plain [| robotoGlyph 0x71u; below; acute |] "dot below (220) sorts before acute (230)"
            use functions = new HarfBuzzSharp.UnicodeFunctions(HarfBuzzSharp.UnicodeFunctions.Default)
            functions.SetCombiningClassDelegate(HarfBuzzSharp.CombiningClassDelegate(fun _ unicode ->
                match unicode with
                | 0x0301u -> enum<HarfBuzzSharp.UnicodeCombiningClass> 220
                | 0x0323u -> enum<HarfBuzzSharp.UnicodeCombiningClass> 230
                | other -> HarfBuzzSharp.UnicodeFunctions.Default.GetCombiningClass other))
            Expect.equal (shaped functions HarfBuzzSharp.Direction.LeftToRight "q̣́") [| robotoGlyph 0x71u; acute; below |] "the delegate swaps them"
        }
        test "a script delegate decides what GuessSegmentProperties finds" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16 "abc"
            buffer.GuessSegmentProperties()
            Expect.equal buffer.Script HarfBuzzSharp.Script.Latin "Latin by default"
            use functions = new HarfBuzzSharp.UnicodeFunctions(HarfBuzzSharp.UnicodeFunctions.Default)
            functions.SetScriptDelegate(HarfBuzzSharp.ScriptDelegate(fun _ _ -> HarfBuzzSharp.Script.Arabic))
            use arabic = new HarfBuzzSharp.Buffer()
            arabic.UnicodeFunctions <- functions
            arabic.AddUtf16 "abc"
            arabic.GuessSegmentProperties()
            Expect.equal arabic.Script HarfBuzzSharp.Script.Arabic "the delegate's script"
            Expect.equal arabic.Direction HarfBuzzSharp.Direction.RightToLeft "the direction of that script"
        }
        test "Default and Empty, parents, immutability and destroy callbacks behave as the native objects" {
            let defaults = HarfBuzzSharp.UnicodeFunctions.Default
            let empty = HarfBuzzSharp.UnicodeFunctions.Empty
            Expect.isTrue defaults.IsImmutable "Default is immutable"
            Expect.isTrue empty.IsImmutable "Empty is immutable"
            Expect.equal (empty.GetGeneralCategory 0x41u) HarfBuzzSharp.UnicodeGeneralCategory.OtherLetter "nil category"
            Expect.equal (empty.GetCombiningClass 0x0301u) HarfBuzzSharp.UnicodeCombiningClass.NotReordered "nil class"
            Expect.equal (empty.GetMirroring 0x28u) 0x28u "nil mirroring"
            Expect.equal (empty.GetScript 0x41u) HarfBuzzSharp.Script.Unknown "nil script"
            Expect.equal (empty.TryCompose(0x61u, 0x0301u)) (false, 0u) "nil compose"
            Expect.equal (empty.TryDecompose 0xE1u) (false, 0xE1u, 0u) "nil decompose leaves the code point in the first output"
            Expect.equal (defaults.TryDecompose 0xE1u) (true, 0x61u, 0x0301u) "default decompose"
            Expect.equal (defaults.TryDecompose 0x41u) (false, 0x41u, 0u) "default decompose of a letter"
            Expect.throwsT<ArgumentNullException> (fun () -> new HarfBuzzSharp.UnicodeFunctions(null) |> ignore) "null parent"
            let error = try defaults.SetScriptDelegate(HarfBuzzSharp.ScriptDelegate(fun _ _ -> HarfBuzzSharp.Script.Latin)); None with :? InvalidOperationException as error -> Some error.Message
            Expect.equal error (Some "UnicodeFunctions is immutable and can't be changed.") "immutable"
            let destroyed = ref 0
            let parent = new HarfBuzzSharp.UnicodeFunctions(empty)
            Expect.throwsT<ArgumentNullException> (fun () -> parent.SetScriptDelegate(Unchecked.defaultof<HarfBuzzSharp.ScriptDelegate>)) "null delegate"
            parent.SetScriptDelegate(HarfBuzzSharp.ScriptDelegate(fun _ _ -> HarfBuzzSharp.Script.Greek), HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 1))
            parent.SetScriptDelegate(HarfBuzzSharp.ScriptDelegate(fun _ _ -> HarfBuzzSharp.Script.Hebrew), HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 10))
            Expect.equal destroyed.Value 1 "the first destroy ran at the replacement"
            Expect.isFalse parent.IsImmutable "mutable until it is a parent"
            let child = new HarfBuzzSharp.UnicodeFunctions(parent)
            Expect.isTrue parent.IsImmutable "a parent is made immutable"
            Expect.isTrue (obj.ReferenceEquals(child.Parent, parent)) "Parent"
            Expect.isNull (defaults.Parent) "Default has no parent"
            Expect.equal (child.GetScript 0x41u) HarfBuzzSharp.Script.Hebrew "the child inherits the delegate"
            Expect.equal (child.GetGeneralCategory 0x41u) HarfBuzzSharp.UnicodeGeneralCategory.OtherLetter "and the nil functions of its parent's base"
            parent.Dispose()
            parent.Dispose()
            Expect.equal destroyed.Value 11 "the second destroy ran once at the dispose"
            child.Dispose()
        }
        test "a buffer has the default functions until it is given others, and Reset restores them" {
            use buffer = new HarfBuzzSharp.Buffer()
            Expect.isTrue (obj.ReferenceEquals(buffer.UnicodeFunctions, HarfBuzzSharp.UnicodeFunctions.Default)) "default"
            use functions = new HarfBuzzSharp.UnicodeFunctions(HarfBuzzSharp.UnicodeFunctions.Default)
            buffer.UnicodeFunctions <- functions
            Expect.isTrue (obj.ReferenceEquals(buffer.UnicodeFunctions, functions)) "set"
            buffer.Reset()
            Expect.isTrue (obj.ReferenceEquals(buffer.UnicodeFunctions, HarfBuzzSharp.UnicodeFunctions.Default)) "reset"
            Expect.throwsT<NullReferenceException> (fun () -> buffer.UnicodeFunctions <- null) "a null value fails as the native binding's value.Handle does"
        }
    ]
