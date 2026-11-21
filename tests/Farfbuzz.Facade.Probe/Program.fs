module Farfbuzz.Facade.Probe.Program

open System
open System.IO
open Farfbuzz.Facade.Consumer

/// <summary>The identity the facade must present to consumers compiled against HarfBuzzSharp.</summary>
[<Literal>]
let ExpectedIdentity = "HarfBuzzSharp, Version=1.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756"

/// <summary>Real HarfBuzzSharp consumers in the local NuGet cache, checked for information when present.</summary>
let externalConsumers =
    let packages = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages")
    [ "Avalonia.Skia 11.3.13", Path.Combine(packages, "avalonia.skia", "11.3.13", "lib", "net8.0", "Avalonia.Skia.dll")
      "Avalonia.HarfBuzz 12.1.1", Path.Combine(packages, "avalonia.harfbuzz", "12.1.1", "lib", "net10.0", "Avalonia.HarfBuzz.dll")
      "SkiaSharp.HarfBuzz 3.116.1", Path.Combine(packages, "skiasharp.harfbuzz", "3.116.1", "lib", "net8.0", "SkiaSharp.HarfBuzz.dll")
      "Topten.RichTextKit 0.4.167", Path.Combine(packages, "topten.richtextkit", "0.4.167", "lib", "net5.0", "Topten.RichTextKit.dll") ]

/// <summary>Formats one check line.</summary>
/// <param name="passed">Whether it passed.</param>
/// <param name="name">What was checked.</param>
/// <param name="detail">What was observed.</param>
/// <returns>The line.</returns>
let line passed name detail = (if passed then "PASS " else "FAIL ") + name + ": " + detail

/// <summary>Runs the gate checks: identity, writes through the glyph span, features, and the consumer's surface.</summary>
/// <param name="args">Unused.</param>
/// <returns>Zero when every gate check passed.</returns>
[<EntryPoint>]
let main args =
    ignore args
    let facade = typeof<HarfBuzzSharp.Buffer>.Assembly
    let identity = facade.FullName |> nonNull
    use probeBuffer = new HarfBuzzSharp.Buffer()
    let isFacade =
        match System.Runtime.InteropServices.GCHandle.FromIntPtr(probeBuffer.Handle).Target with
        | :? HarfBuzzSharp.Buffer as target -> obj.ReferenceEquals(target, probeBuffer)
        | _ -> false
    let written = AvaloniaMirror.WriteThroughGlyphSpan "ab\U0001F600c"
    let features = AvaloniaMirror.Features()
    let consumerPath = typeof<AvaloniaMirror>.Assembly.Location
    let struct (consumerReferences, consumerMissing) =
        match consumerPath.Length with
        | 0 -> struct (0, [])
        | _ -> SurfaceCheck.check facade consumerPath
    let gate =
        [ identity = ExpectedIdentity && isFacade, "identity", identity
          written = "4;42:1F600;42;LeftToRight", "writes through GetGlyphInfoSpan land in the buffer", written
          features = "6C696761;0;3;5", "Feature and Tag", features
          consumerPath.Length = 0 || consumerMissing.IsEmpty, "every HarfBuzzSharp reference of the Avalonia mirror resolves",
          (if consumerPath.Length = 0 then "skipped: single-file or browser build" else $"{consumerReferences} references, {consumerMissing.Length} missing") ]
    gate |> List.iter (fun (passed, name, detail) -> Console.WriteLine(line passed name detail))
    consumerMissing |> List.iter (fun reference -> Console.WriteLine $"    missing {reference.Type}::{reference.Member} {reference.Signature}")
    externalConsumers
    |> List.filter (fun (_, path) -> consumerPath.Length > 0 && File.Exists path)
    |> List.iter (fun (name, path) ->
        let struct (count, missing) = SurfaceCheck.check facade path
        Console.WriteLine $"INFO {name}: {count} HarfBuzzSharp references, {count - missing.Length} resolve in the facade"
        missing |> List.iter (fun reference -> Console.WriteLine $"    missing {reference.Type}::{reference.Member} {reference.Signature}"))
    match gate |> List.forall (fun (passed, _, _) -> passed) with
    | true -> 0
    | false -> 1
