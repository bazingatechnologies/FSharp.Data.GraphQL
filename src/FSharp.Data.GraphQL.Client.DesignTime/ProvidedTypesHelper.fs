/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Validation
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast.Extensions
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
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
        let typ = instance.GetType()
        let fields = FSharpValue.GetRecordFields(instance)
        let fieldInfo = FSharpType.GetRecordFields(typ)
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        typ, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

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
      Alias : string option
      Description : string option
      DeprecationReason : string option
      Type : Type }
    member x.AliasOrName =
        match x.Alias with
        | Some x -> x
        | None -> x.Name

type internal ProvidedRecordTypeDefinition(className, baseType) =
    inherit ProvidedTypeDefinition(className, baseType, nonNullable = true)

    let mutable properties : RecordPropertyMetadata list = []

    member __.GetRecordProperties() = properties

    member __.SetRecordProperties(props) = properties <- props

[<AutoOpen>]
module internal Failures =
    let uploadTypeIsNotScalar uploadTypeName =
        failwithf "Upload type \"%s\" was found on the schema, but it is not a Scalar type. Upload types can only be used if they are defined as scalar types." uploadTypeName

module internal ProvidedRecord =
    let ctor = typeof<RecordBase>.GetConstructors().[0]

    let makeProvidedType(tdef : ProvidedRecordTypeDefinition, properties : RecordPropertyMetadata list) =
        let name = tdef.Name
        tdef.AddMembersDelayed(fun _ -> 
            properties |> List.map (fun metadata ->
                let pname = metadata.AliasOrName.FirstCharUpper()
                let getterCode (args : Expr list) =
                    <@@ let this = %%args.[0] : RecordBase
                        match this.GetProperties() |> List.tryFind (fun prop -> prop.Name = pname) with
                        | Some prop -> prop.Value
                        | None -> failwithf "Expected to find property \"%s\", but the property was not found." pname @@>
                let pdef = ProvidedProperty(pname, metadata.Type, getterCode)
                metadata.Description |> Option.iter pdef.AddXmlDoc
                metadata.DeprecationReason |> Option.iter pdef.AddObsoleteAttribute
                pdef))
        let addConstructorDelayed (propertiesGetter : unit -> (string * string option * Type) list) =
            tdef.AddMembersDelayed(fun _ ->
                // We need to build a constructor overload for each optional property.
                // Since RecordBase needs to know each property information in its own constructor,
                // We also need to know beforehanded each property that was not filled in the current
                // used overload. So we make combinations of all possible overloads, and for each one,
                // we map the user's provided values and we fill the others with a null value.
                // This way we can construct the RecordBase type providing all needed properties.
                // We need to do this because optional parameters have issues with non-nullable types
                // in the type provider SDK. They require a default value, and if the type is not nullable
                // it provides the type default value for it.
                let properties = propertiesGetter()
                let optionalProperties, requiredProperties = 
                    properties 
                    |> List.map (fun (name, alias, t) -> Option.defaultValue name alias, t) 
                    |> List.partition (fun (_, t) -> isOption t)
                List.combinations optionalProperties
                |> List.map (fun (optionalProperties, nullValuedProperties) ->
                    let constructorProperties = requiredProperties @ optionalProperties
                    let propertyNames = (constructorProperties @ nullValuedProperties) |> List.map (fst >> (fun x -> x.FirstCharUpper()))
                    let constructorPropertyTypes = constructorProperties |> List.map snd
                    let nullValuedPropertyTypes = nullValuedProperties |> List.map snd
                    let invoker (args : Expr list) =
                        let properties =
                            let baseConstructorArgs =
                                let coercedArgs = 
                                    (constructorPropertyTypes, args)
                                    ||> List.map2 (fun t arg ->
                                        let arg = Expr.Coerce(arg, typeof<obj>)
                                        if isOption t then <@@ makeSome %%arg @@> else <@@ %%arg @@>)
                                let nullValuedArgs = nullValuedPropertyTypes |> List.map (fun _ -> <@@ null @@>)
                                (propertyNames, (coercedArgs @ nullValuedArgs))
                                ||> List.map2 (fun name value -> <@@ { RecordProperty.Name = name; Value = %%value } @@>)
                            Expr.NewArray(typeof<RecordProperty>, baseConstructorArgs)
                        Expr.NewObject(ctor, [Expr.Value(name); properties])
                    let constructorParams = 
                        constructorProperties
                        |> List.map (fun (name, t) ->
                            match t with
                            | Option t -> ProvidedParameter(name, t)
                            | _ -> ProvidedParameter(name, t))
                    ProvidedConstructor(constructorParams, invoker)))
        match tdef.BaseType with
        | :? ProvidedRecordTypeDefinition as bdef ->
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
            let propertiesGetter() = bdef.GetRecordProperties() @ properties |> List.map (fun p -> p.Name, p.Alias, p.Type)
            addConstructorDelayed propertiesGetter
        | _ -> 
            let propertiesGetter() = properties |> List.map (fun p -> p.Name, p.Alias, p.Type)
            addConstructorDelayed propertiesGetter
        tdef.SetRecordProperties(properties)
        tdef

    let preBuildProvidedType(metadata : ProvidedTypeMetadata, baseType : Type option) =
        let baseType = Option.defaultValue typeof<RecordBase> baseType
        let name = metadata.Name.FirstCharUpper()
        let tdef = ProvidedRecordTypeDefinition(name, Some baseType)
        tdef

#nowarn "10001"

module internal ProvidedOperationResult =
    let makeProvidedType(operationType : Type) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        tdef.AddMemberDelayed(fun _ ->
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : OperationResultBase
                    this.RawData @@>
            let prop = ProvidedProperty("Data", operationType, getterCode)
            prop.AddXmlDoc("Contains the data returned by the operation on the server.")
            prop)
        tdef

module internal ProvidedOperation =
    let makeProvidedType(actualQuery : string,
                         operationDefinition : OperationDefinition,
                         operationTypeName : string,
                         operationFieldsExpr : Expr,
                         schemaTypes: Map<string,IntrospectionType>,
                         schemaProvidedTypes : Map<string, ProvidedTypeDefinition>,
                         operationType : Type,
                         contextInfo : GraphQLRuntimeContextInfo option,
                         uploadInputTypeName : string option,
                         className : string) =
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
        tdef.AddMembersDelayed(fun _ ->
            let operationResultDef = ProvidedOperationResult.makeProvidedType(operationType)
            let isScalar (typeName: string) =
                match schemaTypes.TryFind typeName with
                | Some introspectionType -> introspectionType.Kind = TypeKind.SCALAR
                | None -> false
            let variables =
                let rec mapVariable (variableName : string) (variableType : InputType) =
                    match variableType with
                    | NamedType typeName ->
                        match uploadInputTypeName with
                        | Some uploadInputTypeName when typeName = uploadInputTypeName -> 
                            variableName, Types.makeOption typeof<Upload>
                        | _ ->
                            match Types.scalar.TryFind(typeName) with
                            | Some t -> variableName, Types.makeOption t
                            | None when isScalar typeName -> variableName, typeof<string option>
                            | None ->
                                match schemaProvidedTypes.TryFind(typeName) with
                                | Some t -> variableName, Types.makeOption t
                                | None -> failwithf "Unable to find variable type \"%s\" in the schema definition." typeName
                    | ListType itype -> 
                        let name, t = mapVariable variableName itype
                        name, t |> Types.makeArray |> Types.makeOption
                    | NonNullType itype -> 
                        let name, t = mapVariable variableName itype
                        name, Types.unwrapOption t
                operationDefinition.VariableDefinitions |> List.map (fun vdef -> mapVariable vdef.VariableName vdef.Type)
            let buildVariablesExprFromArgs (varNames : string list) (args : Expr list) =
                let mapVariableExpr (name : string) (value : Expr) =
                    let value = Expr.Coerce(value, typeof<obj>)
                    <@@ let rec mapVariableValue (value : obj) =
                            match value with
                            | null -> null
                            | :? string -> value // We need this because strings are enumerables, and we don't want to enumerate them recursively as an object
                            | :? EnumBase as v -> v.GetValue() |> box
                            | :? RecordBase as v -> v.ToDictionary() |> box
                            | OptionValue v -> v |> Option.map mapVariableValue |> box
                            | EnumerableValue v -> v |> Array.map mapVariableValue |> box
                            | v -> v
                        (name, mapVariableValue %%value) @@>
                let args =
                    let varArgs = List.skip (args.Length - variables.Length) args
                    (varNames, varArgs) ||> List.map2 mapVariableExpr
                Expr.NewArray(typeof<string * obj>, args)
            let defaultContextExpr = 
                match contextInfo with
                | Some info -> 
                    let serverUrl = info.ServerUrl
                    let headerNames = info.HttpHeaders |> Seq.map fst |> Array.ofSeq
                    let headerValues = info.HttpHeaders |> Seq.map snd |> Array.ofSeq
                    <@@ { ServerUrl = serverUrl; HttpHeaders = Array.zip headerNames headerValues } @@>
                | None -> <@@ Unchecked.defaultof<GraphQLProviderRuntimeContext> @@>
            // We need to use the combination strategy to generate overloads for variables in the Run/AsyncRun methods.
            // The strategy follows the same principle with ProvidedRecord constructor overloads,
            // except that we also must create one additional overload for the runtime context, for each already existent overload,
            // if no default context is provided.
            let methodOverloadDefinitions =
                let overloadsWithoutContext =
                    let optionalVariables, requiredVariables = 
                        variables |> List.partition (fun (_, t) -> isOption t)
                    List.combinations optionalVariables 
                    |> List.map (fun (optionalVariables, _) ->
                        let optionalVariables = optionalVariables |> List.map (fun (name, t) -> name, (Types.unwrapOption t))
                        requiredVariables @ optionalVariables)
                let overloadsWithContext = 
                    overloadsWithoutContext
                    |> List.map (fun var -> ("runtimeContext", typeof<GraphQLProviderRuntimeContext>) :: var)
                match contextInfo with
                | Some _ -> overloadsWithoutContext @ overloadsWithContext
                | None -> overloadsWithContext
            // Multipart requests should only be used when the user specifies a upload type name AND the type
            // is present in the query as an input value. If not, we fallback to classic requests.
            let shouldUseMultipartRequest =
                let rec existsUploadType (foundTypes : ProvidedTypeDefinition list) (t : Type) =
                    match t with
                    | :? ProvidedTypeDefinition as tdef when not (List.contains tdef foundTypes) -> tdef.DeclaredProperties |> Seq.exists ((fun p -> p.PropertyType) >> existsUploadType (tdef :: foundTypes))
                    | Option t -> existsUploadType foundTypes t
                    | Array t -> existsUploadType foundTypes t
                    | _ -> t = typeof<Upload>
                variables |> Seq.exists (snd >> existsUploadType [])
            let runMethodOverloads : MemberInfo list = 
                let operationName = Option.toObj operationDefinition.Name
                methodOverloadDefinitions |> List.map (fun overloadParameters ->
                    let variableNames = overloadParameters |> List.map fst |> List.filter (fun name -> name <> "runtimeContext")
                    let invoker (args : Expr list) =
                        // First arg is the operation instance, second should be the context, if the overload asks for one.
                        // We determine it by seeing if the variable names have one less item than the arguments without the instance.
                        let argsWithoutInstance = args.Tail
                        let variableArgs, isDefaultContext, context = 
                            if argsWithoutInstance.Length - variableNames.Length = 1
                            then argsWithoutInstance.Tail, false, argsWithoutInstance.Head
                            else argsWithoutInstance, true, defaultContextExpr
                        let variables = buildVariablesExprFromArgs variableNames variableArgs
                        <@@ 
                            let context = %%context : GraphQLProviderRuntimeContext
                            let request =
                                { ServerUrl = context.ServerUrl
                                  HttpHeaders = context.HttpHeaders
                                  OperationName = Option.ofObj operationName
                                  Query = actualQuery
                                  Variables = %%variables }
                            let response = 
                                if shouldUseMultipartRequest
                                then Tracer.runAndMeasureExecutionTime "Ran a multipart GraphQL query request" (fun _ -> GraphQLClient.sendMultipartRequest context.Connection request)
                                else Tracer.runAndMeasureExecutionTime "Ran a GraphQL query request" (fun _ -> GraphQLClient.sendRequest context.Connection request)
                            let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                            // If the user does not provide a context, we should dispose the default one after running the query
                            if isDefaultContext then (context :> IDisposable).Dispose()
                            OperationResultBase(responseJson, %%operationFieldsExpr, operationTypeName) @@>
                    let methodParameters = overloadParameters |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                    let methodDef = ProvidedMethod("Run", methodParameters, operationResultDef, invoker)
                    methodDef.AddXmlDoc("Executes the operation on the server and fetch its results.")
                    upcast methodDef)
            let asyncRunMethodOverloads : MemberInfo list = 
                let operationName = Option.toObj operationDefinition.Name
                methodOverloadDefinitions |> List.map (fun overloadParameters ->
                    let variableNames = overloadParameters |> List.map fst |> List.filter (fun name -> name <> "runtimeContext")
                    let invoker (args : Expr list) =
                        // First arg is the operation instance, second should be the context, if the overload asks for one.
                        // We determine it by seeing if the variable names have one less item than the arguments without the instance.
                        let argsWithoutInstance = args.Tail
                        let variableArgs, isDefaultContext, context = 
                            if argsWithoutInstance.Length - variableNames.Length = 1
                            then argsWithoutInstance.Tail, false, argsWithoutInstance.Head
                            else argsWithoutInstance, true, defaultContextExpr
                        let variables = buildVariablesExprFromArgs variableNames variableArgs
                        <@@ let context = %%context : GraphQLProviderRuntimeContext
                            let request =
                                { ServerUrl = context.ServerUrl
                                  HttpHeaders = context.HttpHeaders
                                  OperationName = Option.ofObj operationName
                                  Query = actualQuery
                                  Variables = %%variables }
                            async {
                                let! response = 
                                    if shouldUseMultipartRequest
                                    then Tracer.asyncRunAndMeasureExecutionTime "Ran a multipart GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendMultipartRequestAsync context.Connection request)
                                    else Tracer.asyncRunAndMeasureExecutionTime "Ran a GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendRequestAsync context.Connection request)
                                let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                                // If the user does not provide a context, we should dispose the default one after running the query
                                if isDefaultContext then (context :> IDisposable).Dispose()
                                return OperationResultBase(responseJson, %%operationFieldsExpr, operationTypeName)
                            } @@>
                    let methodParameters = overloadParameters |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                    let methodDef = ProvidedMethod("AsyncRun", methodParameters, Types.makeAsync operationResultDef, invoker)
                    methodDef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
                    upcast methodDef)
            let parseResultDef =
                let invoker (args : Expr list) = <@@ OperationResultBase(JsonValue.Parse %%args.[1], %%operationFieldsExpr, operationTypeName) @@>
                let parameters = [ProvidedParameter("responseJson", typeof<string>)]
                let methodDef = ProvidedMethod("ParseResult", parameters, operationResultDef, invoker)
                methodDef.AddXmlDoc("Parses a JSON response that matches the response pattern of the current operation into a OperationResult type.")
                methodDef
            let members : MemberInfo list = [operationResultDef; parseResultDef] @ runMethodOverloads @ asyncRunMethodOverloads
            members)
        tdef

type internal ProvidedOperationMetadata =
    { OperationType : Type
      UploadInputTypeName : string option
      TypeWrapper : ProvidedTypeDefinition }

module internal Provider =
    let getOperationMetadata (schemaTypes : Map<TypeName, IntrospectionType>, uploadInputTypeName : string option, enumProvidedTypes : Map<TypeName, ProvidedTypeDefinition>, operationAstFields, operationTypeRef) =
        let generateWrapper name = 
            let rec resolveWrapperName actual =
                if schemaTypes.ContainsKey(actual)
                then resolveWrapperName (actual + "Fields")
                else actual
            ProvidedTypeDefinition(resolveWrapperName name, None, isSealed = true)
        let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
        let rootWrapper = generateWrapper "Types"
        wrappersByPath.Add([], rootWrapper)
        let rec getWrapper (path : string list) =
            if wrappersByPath.ContainsKey path
            then wrappersByPath.[path]
            else
                let wrapper = generateWrapper (path.Head.FirstCharUpper() + "Fields")
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
        let providedTypes = Dictionary<Path * TypeName, ProvidedTypeDefinition>()
        let rec getProvidedType (providedTypes : Dictionary<Path * TypeName, ProvidedTypeDefinition>) (schemaTypes : Map<TypeName, IntrospectionType>) (path : Path) (astFields : AstFieldInfo list) (tref : IntrospectionTypeRef) : Type =
            match tref.Kind with
            | TypeKind.SCALAR when tref.Name.IsSome -> Types.mapScalarType uploadInputTypeName tref.Name.Value |> Types.makeOption
            | _ when uploadInputTypeName.IsSome && tref.Name.IsSome && uploadInputTypeName.Value = tref.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
            | TypeKind.NON_NULL when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.unwrapOption
            | TypeKind.LIST when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.makeArray |> Types.makeOption
            | TypeKind.ENUM when tref.Name.IsSome ->
                match enumProvidedTypes.TryFind(tref.Name.Value) with
                | Some providedEnum -> Types.makeOption providedEnum
                | None -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION) when tref.Name.IsSome ->
                if providedTypes.ContainsKey(path, tref.Name.Value)
                then Types.makeOption providedTypes.[path, tref.Name.Value]
                else
                    let getIntrospectionFields typeName =
                        if schemaTypes.ContainsKey(typeName)
                        then schemaTypes.[typeName].Fields |> Option.defaultValue [||]
                        else failwithf "Could not find a schema type based on a type reference. The reference is to a \"%s\" type, but that type was not found in the schema types." typeName
                    let getPropertyMetadata typeName (info : AstFieldInfo) : RecordPropertyMetadata =
                        let ifield =
                            match getIntrospectionFields typeName |> Array.tryFind(fun f -> f.Name = info.Name) with
                            | Some ifield -> ifield
                            | None -> failwithf "Could not find field \"%s\" of type \"%s\". The schema type does not have a field with the specified name." info.Name tref.Name.Value
                        let path = info.AliasOrName :: path
                        let astFields = info.Fields
                        let ftype = getProvidedType providedTypes schemaTypes path astFields ifield.Type
                        { Name = info.Name; Alias = info.Alias; Description = ifield.Description; DeprecationReason = ifield.DeprecationReason; Type = ftype }
                    let fragmentProperties =
                        astFields
                        |> List.choose (function FragmentField f when f.TypeCondition <> tref.Name.Value -> Some f | _ -> None)
                        |> List.groupBy (fun field -> field.TypeCondition)
                        |> List.map (fun (typeCondition, fields) ->
                            let conditionFields = fields |> List.distinctBy (fun x -> x.AliasOrName) |> List.map FragmentField
                            typeCondition, List.map (getPropertyMetadata typeCondition) conditionFields)
                    let baseProperties =
                        astFields 
                        |> List.choose (fun x ->
                            match x with
                            | TypeField _ -> Some x
                            | FragmentField f when f.TypeCondition = tref.Name.Value -> Some x
                            | _ -> None)
                        |> List.distinctBy (fun x -> x.AliasOrName)
                        |> List.map (getPropertyMetadata tref.Name.Value)
                    let baseType =
                        let metadata : ProvidedTypeMetadata = { Name = tref.Name.Value; Description = tref.Description }
                        let tdef = ProvidedRecord.preBuildProvidedType(metadata, None)
                        providedTypes.Add((path, tref.Name.Value), tdef)
                        includeType path tdef
                        ProvidedRecord.makeProvidedType(tdef, baseProperties)
                    let createFragmentType (typeName, properties) =
                        let itype =
                            if schemaTypes.ContainsKey(typeName)
                            then schemaTypes.[typeName]
                            else failwithf "Could not find schema type based on the query. Type \"%s\" does not exist on the schema definition." typeName
                        let metadata : ProvidedTypeMetadata = { Name = itype.Name; Description = itype.Description }
                        let tdef = ProvidedRecord.preBuildProvidedType(metadata, Some (upcast baseType))
                        providedTypes.Add((path, typeName), tdef)
                        includeType path tdef
                        ProvidedRecord.makeProvidedType(tdef, properties) |> ignore
                    fragmentProperties |> List.iter createFragmentType
                    Types.makeOption baseType
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        let operationType = getProvidedType providedTypes schemaTypes [] operationAstFields operationTypeRef
        { OperationType = operationType
          UploadInputTypeName = uploadInputTypeName
          TypeWrapper = rootWrapper }

    let getSchemaProvidedTypes(schema : IntrospectionSchema, uploadInputTypeName : string option) =
        let providedTypes = ref Map.empty<TypeName, ProvidedTypeDefinition>
        let schemaTypes = Types.getSchemaTypes schema
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
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType = Types.mapScalarType uploadInputTypeName field.Type.Name.Value
                { Name = field.Name
                  Alias = None
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | _ when uploadInputTypeName.IsSome && field.Type.Name.IsSome && uploadInputTypeName.Value = field.Type.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofFieldType field |> resolveFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofFieldType field |> resolveFieldMetadata |> makeArrayOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Alias = None
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and resolveInputFieldMetadata (field : IntrospectionInputVal) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType = Types.mapScalarType uploadInputTypeName field.Type.Name.Value
                { Name = field.Name
                  Alias = None
                  Description = field.Description
                  DeprecationReason = None
                  Type = providedType }
                |> makeOption
            | _ when uploadInputTypeName.IsSome && field.Type.Name.IsSome && uploadInputTypeName.Value = field.Type.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofInputFieldType field |> resolveInputFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofInputFieldType field |> resolveInputFieldMetadata |> makeArrayOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Alias = None
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
                    upcast ProvidedRecord.makeProvidedType(tdef, properties)
                | TypeKind.INPUT_OBJECT ->
                    let tdef = ProvidedRecord.preBuildProvidedType(metadata, None)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    let properties = 
                        itype.InputFields
                        |> Option.defaultValue [||]
                        |> Array.map resolveInputFieldMetadata
                        |> List.ofArray
                    upcast ProvidedRecord.makeProvidedType(tdef, properties)
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
        |> Seq.iter (fun kvp ->
            if kvp.Value.Kind = TypeKind.INTERFACE || kvp.Value.Kind = TypeKind.UNION then
                let itype = getProvidedType kvp.Value.Name
                let ptypes = possibleTypes kvp.Value
                ptypes |> Array.iter (fun ptype -> ptype.AddInterfaceImplementation(itype)))
        !providedTypes

    let makeProvidedType(asm : Assembly, ns : string, resolutionFolder : string) =
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let staticParams = 
            [ ProvidedStaticParameter("introspection", typeof<string>)
              ProvidedStaticParameter("httpHeaders", typeof<string>, parameterDefaultValue = "")  
              ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder)
              ProvidedStaticParameter("uploadInputTypeName", typeof<string>, parameterDefaultValue = "")
              ProvidedStaticParameter("clientQueryValidation", typeof<bool>, parameterDefaultValue = true) ]
        generator.DefineStaticParameters(staticParams, fun tname args ->
            let clientQueryValidation : bool = downcast args.[4]
            let introspectionLocation = IntrospectionLocation.Create(downcast args.[0], downcast args.[2])
            let httpHeadersLocation = StringLocation.Create(downcast args.[1], resolutionFolder)
            let uploadInputTypeName = 
                let name : string = unbox args.[3]
                match name with
                | null | "" -> None
                | _ -> Some name
            let maker =
                lazy
                    let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
                    tdef.AddXmlDoc("A type provider for GraphQL operations.")
                    tdef.AddMembersDelayed (fun _ ->
                        let httpHeaders = HttpHeaders.load httpHeadersLocation
                        let schema =
                            match introspectionLocation with
                            | Uri serverUrl -> 
                                use connection = new GraphQLClientConnection()
                                GraphQLClient.getIntrospection connection serverUrl httpHeaders
                            | IntrospectionFile path ->
                                path |> System.IO.File.ReadAllText |> Serialization.deserializeSchema
                        let schemaProvidedTypes = getSchemaProvidedTypes(schema, uploadInputTypeName)
                        let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
                        typeWrapper.AddMembers(schemaProvidedTypes |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq)
                        let operationWrapper = ProvidedTypeDefinition("Operations", None, isSealed = true)
                        let getContextMethodDef =
                            let methodParameters =
                                let serverUrl =
                                    match introspectionLocation with
                                    | Uri serverUrl -> ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)
                                    | _ -> ProvidedParameter("serverUrl", typeof<string>)
                                let httpHeaders = ProvidedParameter("httpHeaders", typeof<seq<string * string>>, optionalValue = null)
                                [serverUrl; httpHeaders]
                            let defaultHttpHeadersExpr =
                                let names = httpHeaders |> Seq.map fst |> Array.ofSeq
                                let values = httpHeaders |> Seq.map snd |> Array.ofSeq
                                Expr.Coerce(<@@ Array.zip names values @@>, typeof<seq<string * string>>)
                            let invoker (args : Expr list) =
                                let serverUrl = args.[0]
                                <@@ let httpHeaders =
                                        match %%args.[1] : seq<string * string> with
                                        | null -> %%defaultHttpHeadersExpr
                                        | argHeaders -> argHeaders
                                    { ServerUrl = %%serverUrl; HttpHeaders = httpHeaders } @@>
                            ProvidedMethod("GetContext", methodParameters, typeof<GraphQLProviderRuntimeContext>, invoker, isStatic = true)
                        let operationMethodDef =
                            let staticParams = 
                                [ ProvidedStaticParameter("query", typeof<string>)
                                  ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder)
                                  ProvidedStaticParameter("operationName", typeof<string>, parameterDefaultValue = "")
                                  ProvidedStaticParameter("typeName", typeof<string>, parameterDefaultValue = "") ]
                            let staticMethodDef = ProvidedMethod("Operation", [], typeof<OperationBase>, isStatic = true)
                            let instanceBuilder (methodName : string) (args : obj []) =
                                let queryLocation = StringLocation.Create(downcast args.[0], downcast args.[1])
                                let query = 
                                    match queryLocation with
                                    | String query -> query
                                    | File path -> System.IO.File.ReadAllText(path)
                                let queryAst = Parser.parse query
                                #if IS_DESIGNTIME
                                let throwExceptionIfValidationFailed (validationResult : ValidationResult<AstError>) =
                                    let rec formatValidationExceptionMessage(errors : AstError list) =
                                        match errors with
                                        | [] -> "Query validation resulted in invalid query, but no validation messages were produced."
                                        | errors ->
                                            errors
                                            |> List.map (fun err ->
                                                match err.Path with
                                                | Some path -> sprintf "%s Path: %A" err.Message path
                                                | None -> err.Message)
                                            |> List.reduce (fun x y -> x + Environment.NewLine + y)
                                    match validationResult with
                                    | ValidationError msgs -> failwith (formatValidationExceptionMessage msgs)
                                    | Success -> ()
                                let key = { DocumentId = queryAst.GetHashCode(); SchemaId = schema.GetHashCode() }
                                let refMaker = lazy Validation.Ast.validateDocument schema queryAst
                                if clientQueryValidation then
                                    refMaker.Force
                                    |> QueryValidationDesignTimeCache.getOrAdd key
                                    |> throwExceptionIfValidationFailed
                                #endif
                                let operationName : OperationName option = 
                                    match args.[2] :?> string with
                                    | null | "" -> 
                                        let operationDefinitions = queryAst.Definitions |> List.filter (function OperationDefinition _ -> true | _ -> false)
                                        match operationDefinitions with
                                        | opdef :: _ -> opdef.Name
                                        | _ -> failwith "Error parsing query. Can not choose a default operation: query document has no operation definitions."
                                    | x -> Some x
                                let explicitOperationTypeName : TypeName option = 
                                    match args.[3] :?> string with
                                    | null | "" -> None   
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
                                let schemaTypes = Types.getSchemaTypes schema
                                let enumProvidedTypes = schemaProvidedTypes |> Map.filter (fun _ t -> t.BaseType = typeof<EnumBase>)
                                let actualQuery = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames).Replace("\r\n", "\n")
                                let className =
                                    match explicitOperationTypeName, operationDefinition.Name with
                                    | Some name, _ -> name.FirstCharUpper()
                                    | None, Some name -> name.FirstCharUpper()
                                    | None, None -> "Operation" + actualQuery.MD5Hash()
                                let metadata = getOperationMetadata(schemaTypes, uploadInputTypeName, enumProvidedTypes, operationAstFields, operationTypeRef)
                                let operationTypeName : TypeName =
                                    match operationTypeRef.Name with
                                    | Some name -> name
                                    | None -> failwith "Error parsing query. Operation type does not have a name."
                                let rec getKind (tref : IntrospectionTypeRef) =
                                    match tref.Kind with
                                    | TypeKind.NON_NULL | TypeKind.LIST  when tref.OfType.IsSome -> getKind tref.OfType.Value
                                    | _ -> tref.Kind
                                let rec getTypeName (tref : IntrospectionTypeRef) =
                                    match tref.Kind with
                                    | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> getTypeName tref.OfType.Value
                                    | _ ->
                                        match tref.Name with
                                        | Some tname -> tname
                                        | None -> failwithf "Expected type kind \"%s\" to have a name, but it does not have a name." (tref.Kind.ToString())
                                let rec getIntrospectionType (tref : IntrospectionTypeRef) =
                                    match tref.Kind with
                                    | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> getIntrospectionType tref.OfType.Value
                                    | _ ->
                                        let typeName = getTypeName tref
                                        match schemaTypes.TryFind(typeName) with
                                        | Some t -> t
                                        | None -> failwithf "Type \"%s\" was not found in the introspection schema." typeName
                                let getOperationFields (operationAstFields : AstFieldInfo list) (operationType : IntrospectionType) =
                                    let rec helper (acc : SchemaFieldInfo list) (astFields : AstFieldInfo list) (introspectionType : IntrospectionType) =
                                        match introspectionType.Kind with
                                        | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
                                            match astFields with
                                            | [] -> acc
                                            | field :: tail ->
                                                let throw typeName = failwithf "Field \"%s\" of type \"%s\" was not found in the introspection schema." field.Name typeName
                                                let tref =
                                                    match field with
                                                    | FragmentField fragf ->
                                                        let fragmentType =
                                                            let tref = 
                                                                Option.defaultValue [||] introspectionType.PossibleTypes
                                                                |> Array.map getIntrospectionType
                                                                |> Array.append [|introspectionType|]
                                                                |> Array.tryFind (fun pt -> pt.Name = fragf.TypeCondition)
                                                            match tref with
                                                            | Some t -> t
                                                            | None -> failwithf "Fragment field defines a type condition \"%s\", but that type was not found in the schema definition." fragf.TypeCondition
                                                        let field =
                                                            fragmentType.Fields
                                                            |> Option.map (Array.tryFind (fun f -> f.Name = fragf.Name)) 
                                                            |> Option.flatten
                                                        match field with
                                                        | Some f -> f.Type
                                                        | None -> throw fragmentType.Name
                                                    | TypeField typef ->
                                                        let field =
                                                            introspectionType.Fields 
                                                            |> Option.map (Array.tryFind (fun f -> f.Name = typef.Name))
                                                            |> Option.flatten
                                                        match field with
                                                        | Some f -> f.Type
                                                        | None -> throw introspectionType.Name
                                                let fields = 
                                                    match getKind tref with
                                                    | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
                                                        let schemaType = getIntrospectionType tref
                                                        helper [] field.Fields schemaType
                                                    | _ -> []
                                                let info = { AliasOrName = field.AliasOrName.FirstCharUpper(); SchemaTypeRef = tref; Fields = Array.ofList fields }
                                                helper (info :: acc) tail introspectionType
                                        | _ -> []
                                    helper [] operationAstFields operationType |> Array.ofList
                                // Every time we run the query, we will need the schema types information as an expression.
                                // To avoid creating the type map expression every time we call Run method, we cache it here.
                                let operationFieldsExpr = getOperationFields operationAstFields (getIntrospectionType operationTypeRef) |> QuotationHelpers.arrayExpr |> snd
                                let contextInfo : GraphQLRuntimeContextInfo option =
                                    match introspectionLocation with
                                    | Uri serverUrl -> Some { ServerUrl = serverUrl; HttpHeaders = httpHeaders }
                                    | _ -> None
                                let operationDef = ProvidedOperation.makeProvidedType(actualQuery, operationDefinition, operationTypeName, operationFieldsExpr, schemaTypes, schemaProvidedTypes, metadata.OperationType, contextInfo, metadata.UploadInputTypeName, className)
                                operationDef.AddMember(metadata.TypeWrapper)
                                let invoker (_ : Expr list) = <@@ OperationBase(query) @@>
                                let methodDef = ProvidedMethod(methodName, [], operationDef, invoker, isStatic = true)
                                methodDef.AddXmlDoc("Creates an operation to be executed on the server and provide its return types.")
                                operationWrapper.AddMember(operationDef)
                                operationDef.AddMember(methodDef)
                                methodDef
                            staticMethodDef.DefineStaticParameters(staticParams, instanceBuilder)
                            staticMethodDef
                        let schemaPropertyDef = 
                            let getter = QuotationHelpers.quoteRecord schema (fun (_ : Expr list) schema -> schema)
                            ProvidedProperty("Schema", typeof<IntrospectionSchema>, getter, isStatic = true)
                        let members : MemberInfo list = [typeWrapper; operationWrapper; getContextMethodDef; operationMethodDef; schemaPropertyDef]
                        members)
                    tdef
            #if IS_DESIGNTIME
            let providerKey = 
                { IntrospectionLocation = introspectionLocation
                  CustomHttpHeadersLocation = httpHeadersLocation
                  UploadInputTypeName = uploadInputTypeName
                  ResolutionFolder = resolutionFolder
                  ClientQueryValidation = clientQueryValidation }
            ProviderDesignTimeCache.getOrAdd providerKey maker.Force)
            #else
            maker.Force())
            #endif
        generator
