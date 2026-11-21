module Farfbuzz.Facade.Probe.SurfaceCheck

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// <summary>The assembly name every reference is checked against.</summary>
[<Literal>]
let TargetAssembly = "HarfBuzzSharp"

/// <summary>One HarfBuzzSharp member or type that a consumer binary references.</summary>
type Reference =
    { Type: string        // full name of the declaring type
      Member: string      // member name, or empty for a type reference
      Signature: string } // return and parameter types, or the field type

/// <summary>Decodes signature blobs into type names spelled the way <see cref="spell"/> spells reflected types.</summary>
/// <param name="reader">The consumer's metadata.</param>
/// <returns>The provider.</returns>
let private provider (reader: MetadataReader) =
    let nameOf (handle: TypeReferenceHandle) =
        let reference = reader.GetTypeReference handle
        let ns = reader.GetString reference.Namespace
        let name = reader.GetString reference.Name
        if ns.Length = 0 then name else ns + "." + name
    { new ISignatureTypeProvider<string, unit> with
        member _.GetPrimitiveType code =
            match code with
            | PrimitiveTypeCode.Void -> "System.Void"
            | PrimitiveTypeCode.Boolean -> "System.Boolean"
            | PrimitiveTypeCode.Char -> "System.Char"
            | PrimitiveTypeCode.SByte -> "System.SByte"
            | PrimitiveTypeCode.Byte -> "System.Byte"
            | PrimitiveTypeCode.Int16 -> "System.Int16"
            | PrimitiveTypeCode.UInt16 -> "System.UInt16"
            | PrimitiveTypeCode.Int32 -> "System.Int32"
            | PrimitiveTypeCode.UInt32 -> "System.UInt32"
            | PrimitiveTypeCode.Int64 -> "System.Int64"
            | PrimitiveTypeCode.UInt64 -> "System.UInt64"
            | PrimitiveTypeCode.Single -> "System.Single"
            | PrimitiveTypeCode.Double -> "System.Double"
            | PrimitiveTypeCode.String -> "System.String"
            | PrimitiveTypeCode.IntPtr -> "System.IntPtr"
            | PrimitiveTypeCode.UIntPtr -> "System.UIntPtr"
            | PrimitiveTypeCode.Object -> "System.Object"
            | PrimitiveTypeCode.TypedReference -> "System.TypedReference"
            | other -> $"primitive {int other}"
        member _.GetTypeFromDefinition(_, handle, _) = (reader.GetString (reader.GetTypeDefinition handle).Name)
        member _.GetTypeFromReference(_, handle, _) = nameOf handle
        member _.GetTypeFromSpecification(_, _, _, _) = "typespec"
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

/// <summary>Spells a reflected type the way the signature provider does.</summary>
/// <param name="t">The type.</param>
/// <returns>The spelling.</returns>
let rec spell (t: Type) : string =
    match t with
    | _ when t.IsByRef -> spell (t.GetElementType() |> nonNull) + "&"
    | _ when t.IsPointer -> spell (t.GetElementType() |> nonNull) + "*"
    | _ when t.IsSZArray -> spell (t.GetElementType() |> nonNull) + "[]"
    | _ when t.IsGenericType ->
        let definition = t.GetGenericTypeDefinition()
        (definition.FullName |> nonNull) + "<" + String.Join(",", t.GetGenericArguments() |> Array.map spell) + ">"
    | _ -> t.FullName |> nonNull

/// <summary>Lists every HarfBuzzSharp type and member a consumer binary references.</summary>
/// <param name="path">The consumer assembly.</param>
/// <returns>The references, distinct.</returns>
let references (path: string) : Reference list =
    use stream = File.OpenRead path
    use pe = new PEReader(stream)
    let reader = pe.GetMetadataReader()
    let decoder = provider reader
    let targetsHarfBuzzSharp (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.AssemblyReference ->
            reader.GetString (reader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit handle)).Name = TargetAssembly
        | _ -> false
    let typeName (handle: TypeReferenceHandle) =
        let reference = reader.GetTypeReference handle
        reader.GetString reference.Namespace + "." + reader.GetString reference.Name
    let types =
        reader.TypeReferences
        |> Seq.filter (fun handle -> targetsHarfBuzzSharp (reader.GetTypeReference handle).ResolutionScope)
        |> Seq.map (fun handle -> { Type = typeName handle; Member = String.Empty; Signature = String.Empty })
        |> Seq.toList
    let members =
        reader.MemberReferences
        |> Seq.choose (fun handle ->
            let reference = reader.GetMemberReference handle
            match reference.Parent.Kind with
            | HandleKind.TypeReference ->
                let parent = TypeReferenceHandle.op_Explicit reference.Parent
                match targetsHarfBuzzSharp (reader.GetTypeReference parent).ResolutionScope with
                | false -> None
                | true ->
                    let signature =
                        match reference.GetKind() with
                        | MemberReferenceKind.Method ->
                            let decoded = reference.DecodeMethodSignature(decoder, ())
                            decoded.ReturnType + "(" + String.Join(",", decoded.ParameterTypes) + ")"
                        | _ -> reference.DecodeFieldSignature(decoder, ())
                    Some { Type = typeName parent; Member = reader.GetString reference.Name; Signature = signature }
            | _ -> None)
        |> Seq.toList
    types @ members |> List.distinct

/// <summary>Tells whether the facade assembly provides a reference with exactly that signature.</summary>
/// <param name="facade">The facade assembly.</param>
/// <param name="reference">The reference.</param>
/// <returns>True when it resolves.</returns>
let resolves (facade: Assembly) (reference: Reference) =
    let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy
    match facade.GetType reference.Type, reference.Member with
    | null, _ -> false
    | _, name when name.Length = 0 -> true
    | declaring, ".ctor" ->
        declaring.GetConstructors flags
        |> Array.exists (fun ctor -> "System.Void(" + String.Join(",", ctor.GetParameters() |> Array.map (fun p -> spell p.ParameterType)) + ")" = reference.Signature)
    | declaring, name ->
        let methods =
            declaring.GetMethods flags
            |> Array.filter (fun m -> m.Name = name)
            |> Array.exists (fun m -> spell m.ReturnType + "(" + String.Join(",", m.GetParameters() |> Array.map (fun p -> spell p.ParameterType)) + ")" = reference.Signature)
        let fields = declaring.GetFields flags |> Array.exists (fun f -> f.Name = name && spell f.FieldType = reference.Signature)
        methods || fields

/// <summary>Checks a consumer binary against the facade.</summary>
/// <param name="facade">The facade assembly.</param>
/// <param name="path">The consumer assembly.</param>
/// <returns>The number of references and those that do not resolve.</returns>
let check (facade: Assembly) (path: string) =
    let all = references path
    struct (all.Length, all |> List.filter (fun reference -> not (resolves facade reference)))
