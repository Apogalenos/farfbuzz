module Farfbuzz.Samples.Skia.Program

open System
open System.IO

/// <summary>Writes both consumers' output for the corpus.</summary>
/// <param name="args">The asset directory and the output path.</param>
/// <returns>Zero.</returns>
[<EntryPoint>]
let main args =
    let output = Consumers.output args[0]
    File.WriteAllText(args[1], String.Join('\n', output) + "\n")
    Console.WriteLine $"{output.Length} lines written to {args[1]}"
    0
