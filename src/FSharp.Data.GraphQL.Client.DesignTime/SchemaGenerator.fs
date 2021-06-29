namespace FSharp.Data.GraphQL


open System
open System.Reflection
open System.Collections.Generic
open FSharp.Core
open FSharp.Quotations
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.ProvidedSchema
open ProviderImplementation.ProvidedTypes


type internal SchemaTypes (schema: IntrospectionSchema, ?uploadInputTypeName: string) =
    let schemaTypes = TypeMapping.getSchemaTypes schema
    let uploadInputType =
        uploadInputTypeName
        |> Option.map (fun name -> Map.tryFind name schemaTypes)
        |> Option.flatten
    do  match uploadInputType with
        | Some(schemaType) when schemaType.Kind = TypeKind.SCALAR -> ()
        | None _ -> ()
        | Some schemaType -> failwithf "Upload type '%s' must be a scalar." schemaType.Name

    member _.UploadInputType = uploadInputType

    member _.Types = schemaTypes

    member _.Introspection = schema

    member _.ContainsType (name: string) =
        schemaTypes.ContainsKey(name)

    member _.TryFindType(name: string) =
        schemaTypes.TryFind(name)

    member _.FindType(name: string) =
        match schemaTypes.TryFind(name) with
        | Some itype -> itype
        | None -> failwithf "Type \"%s\" was not found on the schema custom types." name

    member this.FindByTypeRef(tref: IntrospectionTypeRef) =
        match tref.Name with
        | Some name -> this.FindType name
        | None -> failwith "Expected schema type to have a name, but it does not have one."

type internal SchemaGenerator (schema: IntrospectionSchema, ?uploadInputTypeName: string, ?explicitOptionalParameters: bool) =
    let explicitOptionalParameters = defaultArg explicitOptionalParameters false
    let schemaTypes = SchemaTypes(schema, ?uploadInputTypeName=uploadInputTypeName)
    let providedTypes = Dictionary<string, ProvidedTypeDefinition>()
    let rec resolveFieldMetadata (field : IntrospectionField) : RecordPropertyMetadata =
        let providedType = resolveByTypeRef field.Type false
        { Name = field.Name
          Alias = None
          Description = field.Description
          DeprecationReason = field.DeprecationReason
          Type = providedType }
    and resolveInputFieldMetadata (field : IntrospectionInputVal) : RecordPropertyMetadata =
        let providedType = resolveByTypeRef field.Type false
        { Name = field.Name
          Alias = None
          Description = field.Description
          DeprecationReason = None
          Type = providedType }
    and resolveByTypeRef (typeRef: IntrospectionTypeRef) (isNonNullable: bool) =
        match typeRef with
        | NamedTypeRef(TypeKind.SCALAR, name) ->
            let scalar = TypeMapping.mapScalarType uploadInputTypeName name
            if isNonNullable
            then scalar
            else TypeMapping.makeOption scalar
        | NamedTypeRef(_, name) ->
            let introspectionType = schemaTypes.FindType name
            let providedType = resolveProvidedType introspectionType :> Type
            if isNonNullable
            then providedType
            else TypeMapping.makeOption providedType
        | ContainerTypeRef(TypeKind.LIST, innerTypeRef) ->
            let innerType = resolveByTypeRef innerTypeRef false
            let listType = TypeMapping.makeArray innerType
            if isNonNullable
            then listType
            else TypeMapping.makeOption listType
        | ContainerTypeRef(TypeKind.NON_NULL, innerTypeRef) ->
            resolveByTypeRef innerTypeRef true
        | typeRef ->
            failwithf "Unsupported Type ref in introspection Schema '%A'" typeRef.Name
    and resolveProvidedType (itype : IntrospectionType) : ProvidedTypeDefinition =
        if providedTypes.ContainsKey(itype.Name)
        then providedTypes.[itype.Name]
        else
            let metadata = { Name = itype.Name; Description = itype.Description }
            match itype.Kind with
            | TypeKind.OBJECT ->
                let tdef = preBuildProvidedRecordType(metadata, None)
                providedTypes.Add(itype.Name, tdef)
                let properties =
                    itype.Fields
                    |> Option.defaultValue [||]
                    |> Array.map resolveFieldMetadata
                    |> List.ofArray
                upcast makeProvidedRecordType(tdef, properties, explicitOptionalParameters)
            | TypeKind.INPUT_OBJECT ->
                let tdef = preBuildProvidedRecordType(metadata, None)
                providedTypes.Add(itype.Name, tdef)
                let properties =
                    itype.InputFields
                    |> Option.defaultValue [||]
                    |> Array.map resolveInputFieldMetadata
                    |> List.ofArray
                upcast makeProvidedRecordType(tdef, properties, explicitOptionalParameters)
            | TypeKind.INTERFACE | TypeKind.UNION ->
                let bdef = makeProvidedInterfaceType(metadata)
                providedTypes.Add(itype.Name, bdef)
                bdef
            | TypeKind.ENUM ->
                let items =
                    match itype.EnumValues with
                    | Some values -> values |> Array.map (fun value -> value.Name)
                    | None -> [||]
                let tdef = makeProvidedEnumType(itype.Name, items)
                providedTypes.Add(itype.Name, tdef)
                tdef
            | _ -> failwithf "Type \"%s\" is not a Record, Union, Enum, Input Object, or Interface type." itype.Name

    do
        for KeyValue(_, schemaType) in schemaTypes.Types do
            let ignoredType =
                schemaType.Kind = TypeKind.NON_NULL ||
                schemaType.Kind = TypeKind.SCALAR ||
                schemaType.Kind = TypeKind.LIST
            if not ignoredType then
                resolveProvidedType schemaType |> ignore

            let isAbstract =
                schemaType.Kind = TypeKind.INTERFACE ||
                schemaType.Kind = TypeKind.UNION
            if isAbstract then
                let itype =
                    match providedTypes.TryGetValue(schemaType.Name) with
                    | true, ptype -> ptype
                    | false, _-> failwithf "Expected to find a type \"%s\" on the schema type map, but it was not found." schemaType.Name
                let possibleTypes =
                    match schemaType.PossibleTypes with
                    | Some trefs -> trefs |> Array.map (schemaTypes.FindByTypeRef >> resolveProvidedType)
                    | None -> [||]
                for possibleType in possibleTypes do
                    possibleType.AddInterfaceImplementation(itype)
    let enumProvidedTypes =
        dict [ for KeyValue(name, providedType) in providedTypes do
                if providedType.BaseType = typeof<EnumBase> then
                    name, providedType ]

    member _.SchemaTypes = schemaTypes
    member _.ProvidedTypes = providedTypes :> IDictionary<_,_>
    member _.EnumProvidedTypes = enumProvidedTypes