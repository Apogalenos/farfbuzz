module Farfbuzz.HarfBuzzSharp.Tests.SkiaConsumersTests

open System
open System.IO
open Expecto
open Farfbuzz.Samples.Skia

/// <summary>The recorded output of the same code compiled against the real HarfBuzzSharp package (HarfBuzz 8.3.1.x bundled with SkiaSharp.HarfBuzz 3.119.1).</summary>
/// <returns>The lines.</returns>
let private recorded () =
    File.ReadAllLines(Path.Combine(AppContext.BaseDirectory, "Baselines", "skia-consumers-native.tsv")) |> Array.toList

/// <summary>The output of the consumers now, over the facade; the published binaries of SkiaSharp.HarfBuzz and Topten.RichTextKit are the ones that run.</summary>
/// <returns>The lines.</returns>
let private current () =
    Consumers.output (Path.Combine(AppContext.BaseDirectory, "Assets"))

/// <summary>The P10.S11 cases: the published SkiaSharp.HarfBuzz and RichTextKit binaries on the facade.</summary>
[<Tests>]
let tests =
    testList "P10.S11" [
        test "the packages' HarfBuzzSharp reference binds to the facade, and no native HarfBuzzSharp library is loaded with it" {
            let assembly = typeof<HarfBuzzSharp.Buffer>.Assembly
            Expect.equal (assembly.GetName().Name) "HarfBuzzSharp" "assembly name"
            Expect.isTrue (assembly.GetReferencedAssemblies() |> Array.exists (fun name -> name.Name = "Farfbuzz.HarfBuzzSharp.Bridge")) "the loaded HarfBuzzSharp is the facade"
            Expect.isFalse (Directory.GetFiles(AppContext.BaseDirectory, "libHarfBuzzSharp*", SearchOption.AllDirectories).Length > 0) "no native library beside the tests"
        }
        test "SKShaper over the facade gives the same codepoints, clusters and points as over native HarfBuzz" {
            let isShaper (line: string) = line.StartsWith "skshaper\t"
            let expected = recorded () |> List.filter isShaper
            let actual = current () |> List.filter isShaper
            Expect.isGreaterThan expected.Length 50 "the baseline has glyphs"
            Expect.equal actual expected "SKShaper lines"
        }
        test "RichTextKit's TextBlock over the facade lays out the same glyphs and positions as over native HarfBuzz" {
            let isRun (line: string) = line.StartsWith "richtextkit\t"
            let expected = recorded () |> List.filter isRun
            let actual = current () |> List.filter isRun
            Expect.equal expected.Length 4 "one line per corpus line"
            Expect.equal actual expected "RichTextKit lines"
        }
    ]
