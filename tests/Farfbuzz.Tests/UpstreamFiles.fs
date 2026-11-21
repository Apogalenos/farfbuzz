module Farfbuzz.Tests.UpstreamFiles

open System
open System.IO
open Farfbuzz.Harness

/// <summary>Reads a file from the pinned upstream submodule; tests may do IO, the library never does.</summary>
/// <param name="relative">The path relative to <c>upstream/harfbuzz</c>.</param>
/// <returns>The file bytes.</returns>
let read (relative: string) : ReadOnlyMemory<byte> =
    File.ReadAllBytes(Path.Combine(RepositoryPaths.upstream (), relative)) |> ReadOnlyMemory
