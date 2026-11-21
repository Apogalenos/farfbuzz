module Farfbuzz.Oracle.OracleLibrary

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Farfbuzz.Harness

/// <summary>The native library name HarfBuzzSharp and our externs bind to.</summary>
[<Literal>]
let LibraryName = "libHarfBuzzSharp"

/// <summary>The HarfBuzz library the differential tests compare against.</summary>
type Oracle =
    /// <summary>HarfBuzz built from the submodule by <c>build.fsx oracle</c>, at the given path.</summary>
    | Built of path: string
    /// <summary>The native library bundled with the HarfBuzzSharp package, chosen with <c>FARFBUZZ_ORACLE=bundled</c>.</summary>
    | Bundled

/// <summary>The file name of the built oracle on this operating system.</summary>
let builtFileName =
    match OperatingSystem.IsWindows(), OperatingSystem.IsMacOS() with
    | true, _ -> "harfbuzz-oracle.dll"
    | false, true -> "libharfbuzz-oracle.dylib"
    | false, false -> "libharfbuzz-oracle.so"

/// <summary>Where <c>build.fsx oracle</c> puts the library for this machine.</summary>
/// <returns>The expected path.</returns>
let defaultBuiltPath () =
    Path.Combine(RepositoryPaths.root (), "artifacts", "oracle", RuntimeInformation.RuntimeIdentifier, builtFileName)

/// <summary>Decides which oracle to use from the environment; a missing build is an error, never a silent substitute.</summary>
/// <param name="mode">The value of <c>FARFBUZZ_ORACLE</c>.</param>
/// <param name="explicitPath">The value of <c>FARFBUZZ_ORACLE_LIB</c>.</param>
/// <param name="defaultPath">The default location of the built library.</param>
/// <param name="exists">Whether a file exists.</param>
/// <returns>The oracle, or why none can be used.</returns>
let choose (mode: string | null) (explicitPath: string | null) (defaultPath: string) (exists: string -> bool) =
    match mode, explicitPath with
    | "bundled", _ -> Ok Bundled
    | _, NonNull path when exists path -> Ok (Built path)
    | _, NonNull path -> Error $"FARFBUZZ_ORACLE_LIB points to {path}, which does not exist."
    | _, Null when exists defaultPath -> Ok (Built defaultPath)
    | _, Null ->
        Error $"No HarfBuzz oracle at {defaultPath}. Build it with `dotnet fsi build.fsx oracle`, or run with FARFBUZZ_ORACLE=bundled to use HarfBuzzSharp's own native library."

/// <summary>Points every <c>libHarfBuzzSharp</c> import of the given assemblies at one loaded library.</summary>
/// <param name="handle">The loaded library.</param>
/// <param name="assemblies">The assemblies whose imports are redirected.</param>
let redirect (handle: nativeint) (assemblies: Assembly list) =
    let resolver = DllImportResolver(fun name _ _ -> if name = LibraryName then handle else IntPtr.Zero)
    assemblies |> List.iter (fun assembly -> NativeLibrary.SetDllImportResolver(assembly, resolver))

/// <summary>Loads the chosen oracle and wires HarfBuzzSharp and this assembly to it. Call once, before any native call.</summary>
/// <returns>The oracle in use, or why none can be used.</returns>
let ensureLoaded () =
    let chosen =
        choose
            (Environment.GetEnvironmentVariable "FARFBUZZ_ORACLE")
            (Environment.GetEnvironmentVariable "FARFBUZZ_ORACLE_LIB")
            (defaultBuiltPath ())
            File.Exists
    match chosen with
    | Ok (Built path) ->
        redirect (NativeLibrary.Load path) [ typeof<HarfBuzzSharp.Buffer>.Assembly; Assembly.GetExecutingAssembly() ]
        Ok (Built path)
    | Ok Bundled -> Ok Bundled
    | Error reason -> Error reason
