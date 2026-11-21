module Farfbuzz.Diff.OracleTests

open System
open Farfbuzz.Oracle
open System.Reflection
open System.Runtime.InteropServices
open Expecto
open Farfbuzz.Oracle.OracleLibrary

/// <summary>Lists the native entry points an assembly imports from <c>libHarfBuzzSharp</c>.</summary>
/// <param name="assembly">The assembly to inspect.</param>
/// <returns>The entry-point names, sorted and distinct.</returns>
let importedEntryPoints (assembly: Assembly) =
    let flags = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
    assembly.GetTypes()
    |> Array.collect (fun declaring -> declaring.GetMethods flags)
    |> Array.filter (fun method -> (method.Attributes &&& MethodAttributes.PinvokeImpl) = MethodAttributes.PinvokeImpl)
    |> Array.choose (fun method ->
        match method.GetCustomAttribute<DllImportAttribute>() with
        | null -> None
        | import when import.Value = LibraryName ->
            match import.EntryPoint with
            | Null -> Some method.Name
            | NonNull entryPoint when entryPoint.Length = 0 -> Some method.Name
            | NonNull entryPoint -> Some entryPoint
        | _ -> None)
    |> Array.distinct
    |> Array.sort

/// <summary>Loads the library the oracle resolves to, for export lookups.</summary>
/// <param name="oracle">The oracle in use.</param>
/// <returns>The library handle.</returns>
let libraryHandle (oracle: Oracle) =
    match oracle with
    | Built path -> NativeLibrary.Load path
    | Bundled -> NativeLibrary.Load(LibraryName, typeof<HarfBuzzSharp.Buffer>.Assembly, Nullable())

/// <summary>Checks on the oracle library itself, P1.Oracle.</summary>
/// <param name="oracle">The oracle chosen at start-up.</param>
/// <returns>The test list.</returns>
let tests (oracle: Oracle) =
    testList "P1.Oracle" [
        test "the oracle is the pinned HarfBuzz release, or the bundled one on request" {
            let expected = match oracle with Built _ -> "15.1.0" | Bundled -> "14.4.0"
            Expect.equal (Native.version ()) expected "hb_version_string"
        }
        test "every native entry point HarfBuzzSharp imports exists in the oracle" {
            let handle = libraryHandle oracle
            let missing =
                importedEntryPoints typeof<HarfBuzzSharp.Buffer>.Assembly
                |> Array.filter (fun name -> not (fst (NativeLibrary.TryGetExport(handle, name))))
            Expect.isEmpty missing "entry points missing from the oracle"
        }
        test "HarfBuzzSharp imports native entry points through libHarfBuzzSharp" {
            Expect.isGreaterThan (importedEntryPoints typeof<HarfBuzzSharp.Buffer>.Assembly).Length 100 "imports found"
        }
        match oracle with
        | Built _ ->
            test "the oracle carries the Farfbuzz shims" {
                Expect.equal (Shims.farfbuzz_oracle_shims_version ()) 1u "shims version"
            }
        | Bundled -> ()
    ]
