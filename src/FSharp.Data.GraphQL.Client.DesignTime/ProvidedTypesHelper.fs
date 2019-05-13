﻿/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Security.Cryptography
open FSharp.Core
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Ast
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast.Extensions
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open System.Reflection
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections

module internal QuotationHelpers = 
    let rec coerceValues fieldTypeLookup fields = 
        let arrayExpr (arrayType : Type) (v : obj) =
            let typ = arrayType.GetElementType()
            let instance =
                match v with
                | :? IEnumerable as x -> Seq.cast<obj> x |> Array.ofSeq
                | _ -> failwith "Unexpected array value."
            let exprs = coerceValues (fun _ -> typ) instance
            Expr.NewArray(typ, exprs)
        let tupleExpr (tupleType : Type) (v : obj) =
            let typ = FSharpType.GetTupleElements tupleType |> Array.mapi (fun i t -> i, t) |> Map.ofArray
            let fieldTypeLookup i = typ.[i]
            let fields = FSharpValue.GetTupleFields v
            let exprs = coerceValues fieldTypeLookup fields
            Expr.NewTuple(exprs)
        Array.mapi (fun i v ->
                let expr = 
                    if isNull v then simpleTypeExpr v
                    else
                        let tpy = v.GetType()
                        if tpy.IsArray then arrayExpr tpy v
                        elif FSharpType.IsTuple tpy then tupleExpr tpy v
                        elif FSharpType.IsUnion tpy then unionExpr v |> snd
                        elif FSharpType.IsRecord tpy then recordExpr v |> snd
                        else simpleTypeExpr v
                Expr.Coerce(expr, fieldTypeLookup i)
        ) fields |> List.ofArray
    
    and simpleTypeExpr instance = Expr.Value(instance)

    and unionExpr instance = 
        let caseInfo, fields = FSharpValue.GetUnionFields(instance, instance.GetType())    
        let fieldInfo = caseInfo.GetFields()
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        caseInfo.DeclaringType, Expr.NewUnionCase(caseInfo, coerceValues fieldTypeLookup fields)

    and recordExpr instance = 
        let tpy = instance.GetType()
        let fields = FSharpValue.GetRecordFields(instance)
        let fieldInfo = FSharpType.GetRecordFields(tpy)
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        tpy, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

    and arrayExpr (instance : 'a array) =
        let typ = typeof<'a>
        let arrayType = instance.GetType()
        let exprs = coerceValues (fun _ -> typ) (instance |> Array.map box)
        arrayType, Expr.NewArray(typ, exprs)

    let createLetExpr varType instance body args = 
        let var = Var("instance", varType)  
        Expr.Let(var, instance, body args (Expr.Var(var)))

    let quoteUnion instance = 
        let func instance = unionExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted union type" (fun _ -> func instance)

    let quoteRecord instance = 
        let func instance = recordExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted record type" (fun _ -> func instance)

    let quoteArray instance = 
        let func instance = arrayExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted array type" (fun _ -> func instance)

module internal ProvidedEnum =
    let ctor = typeof<EnumBase>.GetConstructors().[0]

    let makeProvidedType(name, items : string seq) =
        let tdef = ProvidedTypeDefinition(name, Some typeof<EnumBase>, nonNullable = true, isSealed = true)
        tdef.AddMembersDelayed(fun _ ->
            items
            |> Seq.map (fun item ->
                let getterCode (_ : Expr list) =
                    Expr.NewObject(ctor, [ <@@ name @@>; <@@ item @@> ])
                ProvidedProperty(item, tdef, getterCode, isStatic = true))
            |> Seq.cast<MemberInfo>
            |> List.ofSeq)
        tdef

type internal ProvidedTypeMetadata =
    { Name : string
      Description : string option }

module internal ProvidedInterface =
    let makeProvidedType(metadata : ProvidedTypeMetadata) =
        let tdef = ProvidedTypeDefinition("I" + metadata.Name.FirstCharUpper(), None, nonNullable = true, isInterface = true)
        metadata.Description |> Option.iter tdef.AddXmlDoc
        tdef

type internal RecordPropertyMetadata =
    { Name : string
      Description : string option
      DeprecationReason : string option
      Type : Type }

module internal ProvidedRecord =
    let ctor = typeof<RecordBase>.GetConstructors().[0]

    let makeProvidedType(tdef : ProvidedTypeDefinition, properties : RecordPropertyMetadata list) =
        let name = tdef.Name
        let propertyMapper (metadata : RecordPropertyMetadata) : MemberInfo =
            let pname = metadata.Name.FirstCharUpper()
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    match this.GetProperties() |> List.tryFind (fun prop -> prop.Name = pname) with
                    | Some prop -> prop.Value
                    | None -> failwithf "Expected to find property \"%s\", but the property was not found." pname @@>
            let pdef = ProvidedProperty(pname, metadata.Type, getterCode)
            metadata.Description |> Option.iter pdef.AddXmlDoc
            metadata.DeprecationReason |> Option.iter pdef.AddObsoleteAttribute
            upcast pdef 
        tdef.AddMembersDelayed(fun _ -> List.map propertyMapper properties)
        let addConstructorDelayed (propertiesGetter : unit -> (string * Type) list) =
            tdef.AddMemberDelayed(fun _ ->
                let properties = propertiesGetter ()
                let prm =
                    let mapper (name : string, t : Type) = 
                        match t with
                        | Option t -> ProvidedParameter(name, t, optionalValue = null)
                        | _ -> ProvidedParameter(name, t)
                    let required = properties |> List.filter (fun (_, t) -> not (isOption t)) |> List.map mapper
                    let optional = properties |> List.filter (fun (_, t) -> isOption t) |> List.map mapper
                    required @ optional
                let invoker (args : Expr list) = 
                    let properties =
                        let args = 
                            let names = properties |> List.map (fun (name, _) -> name.FirstCharUpper())
                            let types = properties |> List.map snd
                            let mapper (name : string, t : Type, value : Expr) =
                                let value = Expr.Coerce(value, typeof<obj>)
                                let isOption = isOption t
                                <@@ let value =
                                        match %%value, isOption with
                                        | null, true -> box None
                                        | OptionValue (Some null), true -> box (Some null)
                                        | OptionValue (Some value), true -> box (makeSome value)
                                        | OptionValue (Some value), false -> value
                                        | OptionValue None, false -> null
                                        | OptionValue None, true -> box None
                                        | value, true -> makeSome value
                                        | value, false -> value
                                    { RecordProperty.Name = name; Value = value } @@>
                            List.zip3 names types args |> List.map mapper
                        Expr.NewArray(typeof<RecordProperty>, args)
                    Expr.NewObject(ctor, [Expr.Value(name); properties])
                ProvidedConstructor(prm, invoker))
        match tdef.BaseType with
        | :? ProvidedTypeDefinition as bdef ->
            bdef.AddMembersDelayed(fun _ ->
                let asType = 
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            if this.GetName() = name then this
                            else failwithf "Expected type to be \"%s\", but it is \"%s\". Make sure to check the type by calling \"Is%s\" method before calling \"As%s\" method." name (this.GetName()) name name @@>
                    ProvidedMethod("As" + name, [], tdef, invoker)
                let tryAsType =
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            if this.GetName() = name then Some this
                            else None @@>
                    ProvidedMethod("TryAs" + name, [], typedefof<_ option>.MakeGenericType(tdef), invoker)
                let isType =
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            this.GetName() = name @@>
                    ProvidedMethod("Is" + name, [], typeof<bool>, invoker)
                let members : MemberInfo list = [asType; tryAsType; isType]
                members)
            let propertiesGetter() =
                let bprops = bdef.GetConstructors().[0].GetParameters() |> Array.map (fun p -> p.Name, p.ParameterType) |> List.ofArray
                let props = properties |> List.map (fun p -> p.Name, p.Type)
                bprops @ props
            addConstructorDelayed propertiesGetter
        | _ -> 
            let propertiesGetter() = properties |> List.map (fun p -> p.Name, p.Type)
            addConstructorDelayed propertiesGetter
        tdef

    let preBuildProvidedType(metadata : ProvidedTypeMetadata, baseType : Type option) =
        let baseType = Option.defaultValue typeof<RecordBase> baseType
        let name = metadata.Name.FirstCharUpper()
        let tdef = ProvidedTypeDefinition(name, Some baseType, nonNullable = true, isSealed = true)
        tdef

    let newObjectExpr(properties : (string * obj) list) =
        let names = properties |> List.map fst
        let values = properties |> List.map snd
        Expr.NewObject(ctor, [ <@@ List.zip names values @@> ])

module internal ProvidedOperationResult =
    let makeProvidedType(operationType : Type) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        tdef.AddMemberDelayed(fun _ ->
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : OperationResultBase
                    this.Data @@>
            let prop = ProvidedProperty("Data", operationType, getterCode)
            prop.AddXmlDoc("Contains the data returned by the operation on the server.")
            prop)
        tdef

module internal ProvidedOperation =
    let makeProvidedType(actualQuery : string,
                         operationDefinition : OperationDefinition,
                         operationTypeName : string,
                         schemaTypesExpr : Expr,
                         schemaProvidedTypes : Map<string, ProvidedTypeDefinition>,
                         operationType : Type,
                         contextInfo : GraphQLRuntimeContextInfo option,
                         className : string) =
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
        tdef.AddMembersDelayed(fun _ ->
            let rtdef = ProvidedOperationResult.makeProvidedType(operationType)
            let variables =
                let rec mapVariable (variableName : string) (vartype : InputType) =
                    match vartype with
                    | NamedType typeName ->
                        match Types.scalar.TryFind(typeName) with
                        | Some t -> (variableName, Types.makeOption t)
                        | None ->
                            match schemaProvidedTypes.TryFind(typeName) with
                            | Some t -> (variableName, Types.makeOption t)
                            | None -> failwithf "Unable to find variable type \"%s\" in the schema definition." typeName
                    | ListType itype -> 
                        let (name, t) = mapVariable variableName itype
                        (name, t |> Types.makeArray |> Types.makeOption)
                    | NonNullType itype -> 
                        let (name, t) = mapVariable variableName itype
                        (name, Types.unwrapOption t)
                operationDefinition.VariableDefinitions |> List.map (fun vdef -> mapVariable vdef.VariableName vdef.Type)
            let varExprMapper (variables : (string * Type) list) (args : Expr list) =
                let exprMapper (name : string, value : Expr) =
                    let value = Expr.Coerce(value, typeof<obj>)
                    <@@ let rec mapper (value : obj) =
                            match value with
                            | null -> null
                            | :? EnumBase as v -> v.GetValue() |> box
                            | OptionValue v -> v |> Option.map mapper |> box
                            | :? RecordBase as v -> v.ToDictionary() |> box
                            | v -> v
                        (name, mapper %%value) @@>
                let args =
                    let names = variables |> List.map fst
                    let args = 
                        match contextInfo with
                        | Some _ ->
                            match args with
                            | _ :: tail when tail.Length > 0 -> List.take (tail.Length - 1) tail
                            | _ -> []
                        | None ->
                            match args with
                            | _ :: _ :: tail when tail.Length > 0 -> tail
                            | _ -> []
                    List.zip names args |> List.map exprMapper
                Expr.NewArray(typeof<string * obj>, args)
            let defaultContextExpr = 
                match contextInfo with
                | Some info -> 
                    let serverUrl = info.ServerUrl
                    let headerNames = info.HttpHeaders |> Seq.map fst |> Array.ofSeq
                    let headerValues = info.HttpHeaders |> Seq.map snd |> Array.ofSeq
                    <@@ { ServerUrl = serverUrl; HttpHeaders = Array.zip headerNames headerValues } @@>
                | None -> <@@ Unchecked.defaultof<GraphQLProviderRuntimeContext> @@>
            let varprm = 
                let mapper (name: string, t : Type) =
                    match t with
                    | Option t -> ProvidedParameter(name, t, optionalValue = null)
                    | _ -> ProvidedParameter(name, t)
                let required = variables |> List.filter (fun (_, t) -> not (isOption t)) |> List.map mapper
                let optional = variables |> List.filter (fun (_, t) -> isOption t) |> List.map mapper
                required @ optional
            let mprm =
                match contextInfo with
                | Some _ -> varprm @ [ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>, optionalValue = null)]
                | None -> ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>) :: varprm
            let rundef = 
                let invoker (args : Expr list) =
                    let operationName = Option.toObj operationDefinition.Name
                    let variables = varExprMapper variables args
                    let argsContext = 
                        match contextInfo with
                        | Some _ -> args.[args.Length - 1]
                        | None -> args.[1]
                    <@@ 
                        let argsContext = %%argsContext : GraphQLProviderRuntimeContext
                        let isDefaultContext = Object.ReferenceEquals(argsContext, null)
                        let context = if isDefaultContext then %%defaultContextExpr else argsContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = actualQuery
                              Variables = %%variables }
                        let response = Tracer.runAndMeasureExecutionTime "Ran a GraphQL query" (fun _ -> GraphQLClient.sendRequest context.Connection request)
                        let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                        // If the user does not provide a context, we should dispose the default one after running the query
                        if isDefaultContext then (context :> IDisposable).Dispose()
                        OperationResultBase(responseJson, %%schemaTypesExpr, operationTypeName) @@>
                let mdef = ProvidedMethod("Run", mprm, rtdef, invoker)
                mdef.AddXmlDoc("Executes the operation on the server and fetch its results.")
                mdef
            let arundef = 
                let invoker (args : Expr list) =
                    let operationName = Option.toObj operationDefinition.Name
                    let variables = varExprMapper variables args
                    let argsContext = 
                        match contextInfo with
                        | Some _ -> args.[args.Length - 1]
                        | None -> args.[1]
                    <@@ let argsContext = %%argsContext : GraphQLProviderRuntimeContext
                        let isDefaultContext = Object.ReferenceEquals(argsContext, null)
                        let context = if isDefaultContext then %%defaultContextExpr else argsContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = actualQuery
                              Variables = %%variables }
                        async {
                            let! response = Tracer.asyncRunAndMeasureExecutionTime "Ran a GraphQL query asynchronously" (fun _ -> GraphQLClient.sendRequestAsync context.Connection request)
                            let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                            // If the user does not provide a context, we should dispose the default one after running the query
                            if isDefaultContext then (context :> IDisposable).Dispose()
                            return OperationResultBase(responseJson, %%schemaTypesExpr, operationTypeName)
                        } @@>
                let mdef = ProvidedMethod("AsyncRun", mprm, Types.makeAsync rtdef, invoker)
                mdef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
                mdef
            let prdef =
                let invoker (args : Expr list) =
                    <@@ let responseJson = JsonValue.Parse %%args.[1]
                        OperationResultBase(responseJson, %%schemaTypesExpr, operationTypeName) @@>
                let prm = [ProvidedParameter("responseJson", typeof<string>)]
                let mdef = ProvidedMethod("ParseResult", prm, rtdef, invoker)
                mdef.AddXmlDoc("Parses a JSON response that matches the response pattern of the current operation into a OperationResult type.")
                mdef
            let members : MemberInfo list = [rtdef; rundef; arundef; prdef]
            members)
        tdef

module internal Provider =
    let getOperationProvidedTypes(schemaTypes : Map<TypeName, IntrospectionType>, enumProvidedTypes : Map<TypeName, ProvidedTypeDefinition>, operationAstFields, operationTypeRef) =
        let providedTypes = ref Map.empty<Path * TypeName, ProvidedTypeDefinition>
        let rec getProvidedType (providedTypes : Map<Path * TypeName, ProvidedTypeDefinition> ref) (schemaTypes : Map<TypeName, IntrospectionType>) (path : Path) (astFields : AstFieldInfo list) (tref : IntrospectionTypeRef) : Type =
            match tref.Kind with
            | TypeKind.NON_NULL when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.unwrapOption
            | TypeKind.LIST when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.makeArray |> Types.makeOption
            | TypeKind.SCALAR when tref.Name.IsSome ->
                if Types.scalar.ContainsKey(tref.Name.Value)
                then Types.scalar.[tref.Name.Value] |> Types.makeOption
                else Types.makeOption typeof<string>
            | TypeKind.ENUM when tref.Name.IsSome ->
                match enumProvidedTypes.TryFind(tref.Name.Value) with
                | Some providedEnum -> Types.makeOption providedEnum
                | None -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION) when tref.Name.IsSome ->
                if (!providedTypes).ContainsKey(path, tref.Name.Value)
                then upcast (!providedTypes).[path, tref.Name.Value]
                else
                    let ifields typeName =
                        if schemaTypes.ContainsKey(typeName)
                        then schemaTypes.[typeName].Fields |> Option.defaultValue [||]
                        else failwithf "Could not find a schema type based on a type reference. The reference is to a \"%s\" type, but that type was not found in the schema types." typeName
                    let getPropertyMetadata typeName (info : AstFieldInfo) : RecordPropertyMetadata =
                        let ifield =
                            match ifields typeName |> Array.tryFind(fun f -> f.Name = info.Name) with
                            | Some ifield -> ifield
                            | None -> failwithf "Could not find field \"%s\" of type \"%s\". The schema type does not have a field with the specified name." info.Name tref.Name.Value
                        let path = info.AliasOrName :: path
                        let astFields = info.Fields
                        let ftype = getProvidedType providedTypes schemaTypes path astFields ifield.Type
                        { Name = info.Name; Description = ifield.Description; DeprecationReason = ifield.DeprecationReason; Type = ftype }
                    let baseType =
                        let metadata : ProvidedTypeMetadata = { Name = tref.Name.Value; Description = tref.Description }
                        let tdef = ProvidedRecord.preBuildProvidedType(metadata, None)
                        providedTypes := (!providedTypes).Add((path, tref.Name.Value), tdef)
                        let properties =
                            astFields
                            |> List.filter (function | TypeField _ -> true | _ -> false)
                            |> List.map (getPropertyMetadata tref.Name.Value)
                        ProvidedRecord.makeProvidedType(tdef, properties)
                    let fragmentProperties =
                        astFields
                        |> List.choose (function FragmentField f -> Some f | _ -> None)
                        |> List.groupBy (fun field -> field.TypeCondition)
                        |> List.map (fun (typeCondition, fields) -> typeCondition, List.map (getPropertyMetadata typeCondition) (List.map FragmentField fields))
                    let fragmentTypes =
                        let createFragmentType (typeName, properties) =
                            let itype =
                                if schemaTypes.ContainsKey(typeName)
                                then schemaTypes.[typeName]
                                else failwithf "Could not find schema type based on the query. Type \"%s\" does not exist on the schema definition." typeName
                            let metadata : ProvidedTypeMetadata = { Name = itype.Name; Description = itype.Description }
                            let tdef = ProvidedRecord.preBuildProvidedType(metadata, Some (upcast baseType))
                            ProvidedRecord.makeProvidedType(tdef, properties)
                        fragmentProperties
                        |> List.map createFragmentType
                    fragmentTypes |> List.iter (fun fragmentType -> providedTypes := (!providedTypes).Add((path, fragmentType.Name), fragmentType))
                    Types.makeOption baseType
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        (getProvidedType providedTypes schemaTypes [] operationAstFields operationTypeRef), !providedTypes

    let getSchemaProvidedTypes(schema : IntrospectionSchema) =
        let providedTypes = ref Map.empty<TypeName, ProvidedTypeDefinition>
        let schemaTypes = Types.getSchemaTypes(schema)
        let getSchemaType (tref : IntrospectionTypeRef) =
            match tref.Name with
            | Some name ->
                match schemaTypes.TryFind(name) with
                | Some itype -> itype
                | None -> failwithf "Type \"%s\" was not found on the schema custom types." name
            | None -> failwith "Expected schema type to have a name, but it does not have one."
        let typeModifier (modifier : Type -> Type) (metadata : RecordPropertyMetadata) = { metadata with Type = modifier metadata.Type }
        let makeOption = typeModifier Types.makeOption
        let makeArrayOption = typeModifier (Types.makeArray >> Types.makeOption)
        let unwrapOption = typeModifier Types.unwrapOption
        let ofFieldType (field : IntrospectionField) = { field with Type = field.Type.OfType.Value }
        let ofInputFieldType (field : IntrospectionInputVal) = { field with Type = field.Type.OfType.Value }
        let rec resolveFieldMetadata (field : IntrospectionField) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofFieldType field |> resolveFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofFieldType field |> resolveFieldMetadata |> makeArrayOption
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType =
                    // Unknown scalar types will be mapped to a string type.
                    if Types.scalar.ContainsKey(field.Type.Name.Value)
                    then Types.scalar.[field.Type.Name.Value]
                    else typeof<string>
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and resolveInputFieldMetadata (field : IntrospectionInputVal) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofInputFieldType field |> resolveInputFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofInputFieldType field |> resolveInputFieldMetadata |> makeArrayOption
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType =
                    if Types.scalar.ContainsKey(field.Type.Name.Value)
                    then Types.scalar.[field.Type.Name.Value]
                    else Types.makeOption typeof<string>
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = None
                  Type = providedType }
                |> makeOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = None
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and resolveProvidedType (itype : IntrospectionType) : ProvidedTypeDefinition =
            if (!providedTypes).ContainsKey(itype.Name)
            then (!providedTypes).[itype.Name]
            else
                let metadata = { Name = itype.Name; Description = itype.Description }
                match itype.Kind with
                | TypeKind.OBJECT ->
                    let tdef = ProvidedRecord.preBuildProvidedType(metadata, None)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    let properties = 
                        itype.Fields
                        |> Option.defaultValue [||]
                        |> Array.map resolveFieldMetadata
                        |> List.ofArray
                    ProvidedRecord.makeProvidedType(tdef, properties)
                | TypeKind.INPUT_OBJECT ->
                    let tdef = ProvidedRecord.preBuildProvidedType(metadata, None)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    let properties = 
                        itype.InputFields
                        |> Option.defaultValue [||]
                        |> Array.map resolveInputFieldMetadata
                        |> List.ofArray
                    ProvidedRecord.makeProvidedType(tdef, properties)
                | TypeKind.INTERFACE | TypeKind.UNION ->
                    let bdef = ProvidedInterface.makeProvidedType(metadata)
                    providedTypes := (!providedTypes).Add(itype.Name, bdef)
                    bdef
                | TypeKind.ENUM ->
                    let items =
                        match itype.EnumValues with
                        | Some values -> values |> Array.map (fun value -> value.Name)
                        | None -> [||]
                    let tdef = ProvidedEnum.makeProvidedType(itype.Name, items)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    tdef
                | _ -> failwithf "Type \"%s\" is not a Record, Union, Enum, Input Object, or Interface type." itype.Name
        let ignoredKinds = [TypeKind.SCALAR; TypeKind.LIST; TypeKind.NON_NULL]
        schemaTypes |> Map.iter (fun _ itype -> if not (List.contains itype.Kind ignoredKinds) then resolveProvidedType itype |> ignore)
        let possibleTypes (itype : IntrospectionType) =
            match itype.PossibleTypes with
            | Some trefs -> trefs |> Array.map (getSchemaType >> resolveProvidedType)
            | None -> [||]
        let getProvidedType typeName =
            match (!providedTypes).TryFind(typeName) with
            | Some ptype -> ptype
            | None -> failwithf "Expected to find a type \"%s\" on the schema type map, but it was not found." typeName
        schemaTypes
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun itype -> itype.Kind = TypeKind.INTERFACE || itype.Kind = TypeKind.UNION)
        |> Seq.map (fun itype -> getProvidedType itype.Name, (possibleTypes itype))
        |> Seq.iter (fun (itype, ptypes) -> ptypes |> Array.iter (fun ptype -> ptype.AddInterfaceImplementation(itype)))
        !providedTypes

    let makeProvidedType(asm : Assembly, ns : string, resolutionFolder : string) =
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = 
            [ ProvidedStaticParameter("introspection", typeof<string>)
              ProvidedStaticParameter("httpHeaders", typeof<string>, parameterDefaultValue = "")  
              ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder) ]
        generator.DefineStaticParameters(prm, fun tname args ->
            let introspectionLocation = IntrospectionLocation.Create(downcast args.[0], downcast args.[2])
            let httpHeadersLocation = StringLocation.Create(downcast args.[1], resolutionFolder)
            let maker =
                lazy
                    let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
                    tdef.AddXmlDoc("A type provider for GraphQL operations.")
                    tdef.AddMembersDelayed (fun _ ->
                        let httpHeaders = HttpHeaders.load httpHeadersLocation
                        let schemaJson =
                            match introspectionLocation with
                            | Uri serverUrl -> 
                                use connection = new GraphQLClientConnection()
                                GraphQLClient.sendIntrospectionRequest connection serverUrl httpHeaders
                            | IntrospectionFile path ->
                                System.IO.File.ReadAllText path
                        let schema = Serialization.deserializeSchema schemaJson
                        let schemaProvidedTypes = getSchemaProvidedTypes(schema)
                        let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
                        typeWrapper.AddMembers(schemaProvidedTypes |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq)
                        let operationWrapper = ProvidedTypeDefinition("Operations", None, isSealed = true)
                        let ctxmdef =
                            let prm =
                                let serverUrl =
                                    match introspectionLocation with
                                    | Uri serverUrl -> ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)
                                    | _ -> ProvidedParameter("serverUrl", typeof<string>)
                                let httpHeaders = ProvidedParameter("httpHeaders", typeof<seq<string * string>>, optionalValue = null)
                                [httpHeaders; serverUrl]
                            let defaultHttpHeadersExpr =
                                let names = httpHeaders |> Seq.map fst |> Array.ofSeq
                                let values = httpHeaders |> Seq.map snd |> Array.ofSeq
                                Expr.Coerce(<@@ Array.zip names values @@>, typeof<seq<string * string>>)
                            let invoker (args : Expr list) =
                                let serverUrl = args.[1]
                                <@@ let httpHeaders =
                                        match %%args.[0] : seq<string * string> with
                                        | null -> %%defaultHttpHeadersExpr
                                        | argHeaders -> argHeaders
                                    { ServerUrl = %%serverUrl; HttpHeaders = httpHeaders } @@>
                            ProvidedMethod("GetContext", prm, typeof<GraphQLProviderRuntimeContext>, invoker, isStatic = true)
                        let omdef =
                            let sprm = 
                                [ ProvidedStaticParameter("query", typeof<string>)
                                  ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder)
                                  ProvidedStaticParameter("operationName", typeof<string>, parameterDefaultValue = "")
                                  ProvidedStaticParameter("typeName", typeof<string>, parameterDefaultValue = "") ]
                            let smdef = ProvidedMethod("Operation", [], typeof<OperationBase>, isStatic = true)
                            let genfn (mname : string) (args : obj []) =
                                let queryLocation = StringLocation.Create(downcast args.[0], downcast args.[1])
                                let query = 
                                    match queryLocation with
                                    | String query -> query
                                    | File path -> System.IO.File.ReadAllText(path)
                                let queryAst = Parser.parse query
                                let operationName : OperationName option = 
                                    match args.[2] :?> string with
                                    | "" -> 
                                        match queryAst.Definitions with
                                        | opdef::_ -> opdef.Name
                                        | _ -> failwith "Error parsing query. Can not choose a default operation: query document has no definitions."
                                    | x -> Some x
                                let explicitOperationTypeName : TypeName option = 
                                    match args.[3] :?> string with
                                    | "" -> None   
                                    | x -> Some x    
                                let operationDefinition =
                                    queryAst.Definitions
                                    |> List.choose (function OperationDefinition odef -> Some odef | _ -> None)
                                    |> List.find (fun d -> d.Name = operationName)
                                let operationAstFields =
                                    let infoMap = queryAst.GetInfoMap()
                                    match infoMap.TryFind(operationName) with
                                    | Some fields -> fields
                                    | None -> failwith "Error parsing query. Could not find field information for requested operation."
                                let operationTypeRef =
                                    let tref =
                                        match operationDefinition.OperationType with
                                        | Query -> schema.QueryType
                                        | Mutation -> 
                                            match schema.MutationType with
                                            | Some tref -> tref
                                            | None -> failwith "The operation is a mutation operation, but the schema does not have a mutation type."
                                        | Subscription -> 
                                            match schema.SubscriptionType with
                                            | Some tref -> tref
                                            | None -> failwithf "The operation is a subscription operation, but the schema does not have a subscription type."
                                    let tinst =
                                        match tref.Name with
                                        | Some name -> schema.Types |> Array.tryFind (fun t -> t.Name = name)
                                        | None -> None
                                    match tinst with
                                    | Some t -> { tref with Kind = t.Kind }
                                    | None -> failwith "The operation was found in the schema, but it does not have a name."
                                let schemaTypes = Types.getSchemaTypes(schema)
                                let enumProvidedTypes = schemaProvidedTypes |> Map.filter (fun _ t -> t.BaseType = typeof<EnumBase>)
                                let actualQuery = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames).Replace("\r\n", "\n")
                                let className =
                                    match explicitOperationTypeName, operationDefinition.Name with
                                    | Some name, _ -> 
                                        name.FirstCharUpper()
                                    | None, Some name -> 
                                        name.FirstCharUpper()
                                    | None, None ->   
                                        let hash = 
                                            Encoding.UTF8.GetBytes(actualQuery)
                                            |> MD5.Create().ComputeHash
                                            |> Array.map (fun x -> x.ToString("x2"))
                                            |> Array.reduce (+)
                                        "Operation" + hash
                                let (operationType, operationTypes) = getOperationProvidedTypes(schemaTypes, enumProvidedTypes, operationAstFields, operationTypeRef)
                                let generateWrapper name = ProvidedTypeDefinition(name, None, isSealed = true)
                                let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
                                let rootWrapper = generateWrapper "Types"
                                wrappersByPath.Add([], rootWrapper)
                                let rec getWrapper (path : string list) =
                                    if wrappersByPath.ContainsKey path
                                    then wrappersByPath.[path]
                                    else
                                        let wrapper = generateWrapper (path.Head.FirstCharUpper())
                                        let upperWrapper =
                                            let path = path.Tail
                                            if wrappersByPath.ContainsKey(path)
                                            then wrappersByPath.[path]
                                            else getWrapper path
                                        upperWrapper.AddMember(wrapper)
                                        wrappersByPath.Add(path, wrapper)
                                        wrapper
                                let includeType (path : string list) (t : ProvidedTypeDefinition) =
                                    let wrapper = getWrapper path
                                    wrapper.AddMember(t)
                                operationTypes |> Map.iter (fun (path, _) t -> includeType path t)
                                let operationTypeName : TypeName =
                                    match operationTypeRef.Name with
                                    | Some name -> name
                                    | None -> failwith "Error parsing query. Operation type does not have a name."
                                // Every time we run the query, we will need the schema type map as an expression.
                                // To avoid creating the type map expression every time we call Run method, we cache it here.
                                let schemaTypes =
                                    let schemaTypeNames = schemaTypes |> Seq.map (fun x -> x.Key) |> Array.ofSeq
                                    let schemaTypesExpr = schemaTypes |> Seq.map (fun x -> x.Value) |> Array.ofSeq |> QuotationHelpers.arrayExpr |> snd
                                    <@@ Array.zip schemaTypeNames (%%schemaTypesExpr : IntrospectionType []) |> Map.ofArray @@>
                                let contextInfo : GraphQLRuntimeContextInfo option =
                                    match introspectionLocation with
                                    | Uri serverUrl -> Some { ServerUrl = serverUrl; HttpHeaders = httpHeaders }
                                    | _ -> None
                                let odef = ProvidedOperation.makeProvidedType(actualQuery, operationDefinition, operationTypeName, schemaTypes, schemaProvidedTypes, operationType, contextInfo, className)
                                odef.AddMember(rootWrapper)
                                let invoker (_ : Expr list) = <@@ OperationBase(query) @@>
                                let mdef = ProvidedMethod(mname, [], odef, invoker, isStatic = true)
                                mdef.AddXmlDoc("Creates an operation to be executed on the server and provide its return types.")
                                operationWrapper.AddMember(odef)
                                odef.AddMember(mdef)
                                mdef
                            smdef.DefineStaticParameters(sprm, genfn)
                            smdef
                        let schemapdef = 
                            let getterCode = QuotationHelpers.quoteRecord schema (fun (_ : Expr list) schema -> schema)
                            ProvidedProperty("Schema", typeof<IntrospectionSchema>, getterCode, isStatic = true)
                        let members : MemberInfo list = [typeWrapper; operationWrapper; ctxmdef; omdef; schemapdef]
                        members)
                    tdef
            let providerKey = { IntrospectionLocation = introspectionLocation; CustomHttpHeadersLocation = httpHeadersLocation }
            DesignTimeCache.getOrAdd providerKey maker.Force)
        generator