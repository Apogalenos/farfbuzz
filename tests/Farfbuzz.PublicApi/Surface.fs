module Farfbuzz.PublicApi.Surface

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// <summary>The full name of a type of the library, nested types after a plus sign.</summary>
/// <param name="reader">The metadata.</param>
/// <param name="handle">The type.</param>
/// <returns>The name.</returns>
let rec private fullNameOf (reader: MetadataReader) (handle: TypeDefinitionHandle) =
    let definition = reader.GetTypeDefinition handle
    match definition.GetDeclaringType().IsNil with
    | true ->
        let ns = reader.GetString definition.Namespace
        match ns.Length with
        | 0 -> reader.GetString definition.Name
        | _ -> ns + "." + reader.GetString definition.Name
    | false -> fullNameOf reader (definition.GetDeclaringType()) + "+" + reader.GetString definition.Name

/// <summary>Spells the types of signatures from the metadata of the library, the way the baseline writes them.</summary>
/// <param name="reader">The metadata.</param>
/// <returns>The provider.</returns>
let private provider (reader: MetadataReader) =
    let fullName (handle: TypeDefinitionHandle) = fullNameOf reader handle
    let referenceName (handle: TypeReferenceHandle) =
        let reference = reader.GetTypeReference handle
        let ns = reader.GetString reference.Namespace
        match ns.Length with
        | 0 -> reader.GetString reference.Name
        | _ -> ns + "." + reader.GetString reference.Name
    { new ISignatureTypeProvider<string, unit> with
        member _.GetPrimitiveType code = $"{code}"
        member _.GetTypeFromDefinition(_, handle, _) = fullName handle
        member _.GetTypeFromReference(_, handle, _) = referenceName handle
        member this.GetTypeFromSpecification(_, state, handle, _) = (reader.GetTypeSpecification handle).DecodeSignature(this, state)
        member _.GetSZArrayType element = element + "[]"
        member _.GetArrayType(element, shape) = element + "[" + String(',', shape.Rank - 1) + "]"
        member _.GetByReferenceType element = element + "&"
        member _.GetPointerType element = element + "*"
        member _.GetGenericInstantiation(generic, arguments) = generic + "<" + String.Join(",", arguments) + ">"
        member _.GetGenericTypeParameter(_, index) = $"!{index}"
        member _.GetGenericMethodParameter(_, index) = $"!!{index}"
        member _.GetFunctionPointerType _ = "fnptr"
        member _.GetModifiedType(_, unmodified, _) = unmodified
        member _.GetPinnedType element = element }

/// <summary>Whether a method, field or type attribute set is visible outside the assembly.</summary>
/// <param name="attributes">The attributes.</param>
/// <returns>True for public and protected.</returns>
let private visibleMethod (attributes: System.Reflection.MethodAttributes) =
    let access = attributes &&& System.Reflection.MethodAttributes.MemberAccessMask
    access = System.Reflection.MethodAttributes.Public || access = System.Reflection.MethodAttributes.Family || access = System.Reflection.MethodAttributes.FamORAssem

/// <summary>The public surface of a library: every public type and its public methods, properties and fields with their signatures, one sorted line each.</summary>
/// <param name="path">The assembly.</param>
/// <returns>The lines, sorted.</returns>
let linesOf (path: string) =
    use stream = File.OpenRead path
    use pe = new PEReader(stream)
    let reader = pe.GetMetadataReader()
    let decoder = provider reader
    let typeLines (handle: TypeDefinitionHandle) =
        let definition = reader.GetTypeDefinition handle
        let visibility = definition.Attributes &&& System.Reflection.TypeAttributes.VisibilityMask
        let isPublic = visibility = System.Reflection.TypeAttributes.Public || visibility = System.Reflection.TypeAttributes.NestedPublic
        match isPublic with
        | false -> []
        | true ->
            let name = fullNameOf reader handle
            let methods =
                [ for methodHandle in definition.GetMethods() do
                    let methodDefinition = reader.GetMethodDefinition methodHandle
                    match visibleMethod methodDefinition.Attributes with
                    | false -> ()
                    | true ->
                        let signature = methodDefinition.DecodeSignature(decoder, ())
                        yield $"method {name}.{reader.GetString methodDefinition.Name}({String.Join(',', signature.ParameterTypes)}) : {signature.ReturnType}" ]
            let fields =
                [ for fieldHandle in definition.GetFields() do
                    let field = reader.GetFieldDefinition fieldHandle
                    let access = field.Attributes &&& System.Reflection.FieldAttributes.FieldAccessMask
                    match access = System.Reflection.FieldAttributes.Public with
                    | false -> ()
                    | true -> yield $"field {name}.{reader.GetString field.Name} : {field.DecodeSignature(decoder, ())}" ]
            $"type {name}" :: methods @ fields
    [ for handle in reader.TypeDefinitions do yield! typeLines handle ]
    |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))

/// <summary>The assembly the surface is read from: the library this project references.</summary>
let libraryPath = typeof<Farfbuzz.Face>.Assembly.Location

/// <summary>The lines of the pinned surface.</summary>
/// <param name="baseline">The baseline file.</param>
/// <returns>The lines.</returns>
let pinned (baseline: string) =
    File.ReadAllLines baseline |> Array.toList

/// <summary>Compares the surface with the baseline.</summary>
/// <param name="baseline">The baseline file.</param>
/// <returns>The lines the library has and the baseline has not, and the lines the baseline has and the library has not.</returns>
let differences (baseline: string) =
    let actual = linesOf libraryPath
    let expected = pinned baseline
    let extra = actual |> List.filter (fun line -> not (List.contains line expected))
    let missing = expected |> List.filter (fun line -> not (List.contains line actual))
    extra, missing
