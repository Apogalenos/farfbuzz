module Farfbuzz.Samples.Avalonia11.Headless.Program

open System
open System.IO
open Avalonia
open Avalonia.Headless
open Avalonia.Skia
open Farfbuzz.Samples.Avalonia11

/// <summary>Shapes the corpus through Avalonia 11.3.13's text shaper on a headless Skia platform and writes one line per glyph.</summary>
/// <param name="args">The output path.</param>
/// <returns>Zero.</returns>
[<EntryPoint>]
let main args =
    AppBuilder.Configure<App>().UseSkia().UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false)).SetupWithoutStarting() |> ignore
    let lines = Corpus.lines |> List.collect Corpus.shape
    let output = String.Join('\n', lines) + "\n"
    Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath args[0]) |> nonNull) |> ignore
    File.WriteAllText(args[0], output)
    Console.WriteLine $"{lines.Length} glyphs written to {args[0]}"
    0
