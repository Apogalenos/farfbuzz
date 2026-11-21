module Farfbuzz.Tests.TailCallTests

open System
open System.IO
open Expecto
open Mono.Cecil
open Mono.Cecil.Cil

/// <summary>A call the compiler marked <c>tail.</c>.</summary>
type TailCall =
    { Assembly: string
      Caller: string
      Callee: string
      ByRefLike: string list }  // the byref-like parameter types of the callee

/// <summary>The byref-like types of the base library that a call can pass.</summary>
let private spans = set [ "System.Span`1"; "System.ReadOnlySpan`1" ]

/// <summary>The byref-like types the given modules define, with the base library's spans.</summary>
/// <param name="modules">The modules.</param>
/// <returns>Their full names.</returns>
let private byRefLikeTypes (modules: ModuleDefinition list) =
    modules
    |> List.collect (fun loaded ->
        loaded.GetTypes()
        |> Seq.filter (fun defined -> defined.CustomAttributes |> Seq.exists (fun attribute -> attribute.AttributeType.Name = "IsByRefLikeAttribute"))
        |> Seq.map _.FullName
        |> Seq.toList)
    |> Set.ofList
    |> Set.union spans

/// <summary>Names the target of a call instruction.</summary>
/// <param name="operand">The operand of the call.</param>
/// <returns>The name.</returns>
let private calleeName (operand: obj) =
    match operand with
    | :? MethodReference as target -> $"{target.DeclaringType.Name}::{target.Name}"
    | _ -> "an indirect call"

/// <summary>The byref-like parameter types of a call target.</summary>
/// <param name="byRefLike">The byref-like type names.</param>
/// <param name="operand">The operand of the call.</param>
/// <returns>The names found among the parameters.</returns>
let private byRefLikeParameters (byRefLike: Set<string>) (operand: obj) =
    match operand with
    | :? IMethodSignature as signature ->
        signature.Parameters
        |> Seq.map (fun parameter -> parameter.ParameterType.GetElementType().FullName)
        |> Seq.filter byRefLike.Contains
        |> Seq.toList
    | _ -> []

/// <summary>Every explicit tail call of a module.</summary>
/// <param name="byRefLike">The byref-like type names.</param>
/// <param name="loaded">The module.</param>
/// <returns>The calls.</returns>
let private tailCalls (byRefLike: Set<string>) (loaded: ModuleDefinition) =
    [ for defined in loaded.GetTypes() do
        for body in defined.Methods |> Seq.filter _.HasBody do
            for instruction in body.Body.Instructions |> Seq.filter (fun instruction -> instruction.OpCode = OpCodes.Tail) do
                yield
                    { Assembly = loaded.Assembly.Name.Name
                      Caller = $"{defined.FullName}::{body.Name}"
                      Callee = calleeName instruction.Next.Operand
                      ByRefLike = byRefLikeParameters byRefLike instruction.Next.Operand } ]

/// <summary>
/// The project assemblies of this build: <c>artifacts/bin/NAME/CONFIGURATION/NAME.dll</c> for every project whose
/// name starts with Farfbuzz, in the configuration this test runs in.
/// </summary>
/// <returns>The paths.</returns>
let private projectAssemblies () =
    let here = DirectoryInfo(AppContext.BaseDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar))
    let configuration = here.Name
    let bin = (here.Parent |> nonNull).Parent |> nonNull
    bin.GetDirectories "Farfbuzz*"
    |> Array.map (fun project -> Path.Combine(project.FullName, configuration, project.Name + ".dll"))
    |> Array.filter File.Exists
    |> Array.toList

/// <summary>Every explicit tail call of this build's project assemblies.</summary>
/// <returns>The calls.</returns>
let private scanned () =
    let modules = projectAssemblies () |> List.map (fun path -> ModuleDefinition.ReadModule path)
    let calls = modules |> List.collect (tailCalls (byRefLikeTypes modules))
    modules |> List.iter (fun loaded -> loaded.Dispose())
    calls

/// <summary>Every method of a module that returns a reference.</summary>
/// <param name="loaded">The module.</param>
/// <returns>The methods, by full name.</returns>
let private referenceReturns (loaded: ModuleDefinition) =
    [ for defined in loaded.GetTypes() do
        for body in defined.Methods |> Seq.filter (fun candidate -> candidate.ReturnType.IsByReference) do
            yield $"{loaded.Assembly.Name.Name}: {defined.FullName}::{body.Name}" ]

/// <summary>
/// Tail calls stay on, and none of them may pass a span. With tail calls on, F# marks the last call of a function
/// <c>tail.</c> also when an argument is a span or another byref-like value, and on .NET 10.0.12 the callee then
/// read where the span's array had been before a collection moved it (BLOCKED.md, 2026-11-10).
/// </summary>
[<Tests>]
let tests =
    testList "TailCalls" [
        test "no explicit tail call passes a span or another byref-like value" {
            let offending =
                scanned ()
                |> List.filter (fun call -> not call.ByRefLike.IsEmpty)
                |> List.map (fun call ->
                    let passed = String.Join(" and ", call.ByRefLike)
                    $"  {call.Assembly}: {call.Caller} ends by calling {call.Callee}, passing {passed}")
            let remedy =
                "Such a call must not be the last call of a function. In src/Farfbuzz, build the span inside the function that reads it "
                + "(pass the array), or end the caller with something else. In tests, the harness and benchmarks, write `|> TailCalls.settle` after the call."
            Expect.isEmpty offending ($"{offending.Length} explicit tail call(s) pass a byref-like value:\n" + String.Join("\n", offending) + "\n" + remedy)
        }
        test "no method of the library returns a reference" {
            let modules = projectAssemblies () |> List.map (fun path -> ModuleDefinition.ReadModule path)
            let returning = modules |> List.filter (fun loaded -> loaded.Assembly.Name.Name = "Farfbuzz") |> List.collect referenceReturns
            modules |> List.iter (fun loaded -> loaded.Dispose())
            let remedy =
                "A function that hands out a reference into an object it keeps leaves its caller holding an interior pointer across calls that "
                + "allocate. On browser-wasm compiled ahead of time the lookup kernel read the wrong lookup that way: the first Duployan pass "
                + "shaped 503,804 glyphs, the others 503,948 (PITFALLS.md). Return a copy of the struct instead."
            Expect.isEmpty returning ($"{returning.Length} method(s) return a reference:\n" + String.Join("\n", returning) + "\n" + remedy)
        }
        test "tail calls are on: the library has explicit tail calls" {
            let inLibrary = scanned () |> List.filter (fun call -> call.Assembly = "Farfbuzz")
            Expect.isGreaterThan inLibrary.Length 0 "Farfbuzz.dll has no explicit tail call: Tailcalls was switched off, or the scan reads nothing"
        }
    ]
