module internal FSharp.Data.GraphQL.ProvidedSchema

open System
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type internal ProvidedTypeMetadata =
    { Name : string
      Description : string option }

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

let enumCtor = typeof<EnumBase>.GetConstructors().[0]
let makeProvidedEnumType (name, items : string seq) =
    let tdef = ProvidedTypeDefinition(name, Some typeof<EnumBase>, nonNullable = true, isSealed = true)
    tdef.AddMembersDelayed(fun _ ->
        items
        |> Seq.map (fun item ->
            let getterCode (_ : Expr list) =
                Expr.NewObject(enumCtor, [ <@@ name @@>; <@@ item @@> ])
            ProvidedProperty(item, tdef, getterCode, isStatic = true))
        |> Seq.cast<MemberInfo>
        |> List.ofSeq)
    tdef

let makeProvidedInterfaceType(metadata : ProvidedTypeMetadata) =
    let tdef = ProvidedTypeDefinition("I" + metadata.Name.FirstCharUpper(), None, nonNullable = true, isInterface = true)
    metadata.Description |> Option.iter tdef.AddXmlDoc
    tdef

type internal ProvidedRecordTypeDefinition(className, baseType) =
    inherit ProvidedTypeDefinition(className, baseType, nonNullable = true)

    let mutable properties : RecordPropertyMetadata list = []

    member __.GetRecordProperties() = properties

    member __.SetRecordProperties(props) = properties <- props


let recordCtor = typeof<RecordBase>.GetConstructors().[0]
let makeProvidedRecordType (tdef : ProvidedRecordTypeDefinition, properties : RecordPropertyMetadata list, explicitOptionalParameters: bool) =
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
            // We need to build a constructor that takes all optional properties wrapped in another option.
            // We need to do this because optional parameters have issues with non-nullable types
            // in the type provider SDK. They require a default value, and if the type is not nullable
            // it provides the type default value for it. So basically, what's needed is three-valued behavior
            // so we can differentiate between no value and null/None.
            //
            // I.e. to not send a value the optional parameter can either be set implicitly (by not providing an
            // argument) or explicitly (`?parameterName = Some None` or `parameterName = None`).
            // To set a value it has to be wrapped in an option: `parameterName = Some argumentValue`
            // (or `?parameterName = Some (Some argumentValue)`).
            //
            // To keep backwards compatibility this constructor is only created if a flag is turned on. Otherwise
            // we keep the previous behavior: We build a constructor overload for each optional property.
            // Since RecordBase needs to know each property information in its own constructor,
            // we also need to know each property that was not filled in the currently used overload. So we make
            // combinations of all possible overloads, and for each one we map the user's provided values and
            // fill the others with a null value. This way we can construct the RecordBase type providing all
            // needed properties.
            let properties = propertiesGetter()
            let optionalProperties, requiredProperties =
                properties
                |> List.map (fun (name, alias, t) -> Option.defaultValue name alias, t)
                |> List.partition (fun (_, t) -> isOption t)
            if explicitOptionalParameters then
                let constructorProperties = requiredProperties @ optionalProperties
                let propertyNames = constructorProperties |> List.map (fst >> (fun x -> x.FirstCharUpper()))
                let constructorPropertyTypes = constructorProperties |> List.map snd
                let invoker (args : Expr list) =
                    let properties =
                        let baseConstructorArgs =
                            let coercedArgs =
                                (constructorPropertyTypes, args)
                                ||> List.map2 (fun t arg ->
                                    let arg = Expr.Coerce(arg, typeof<obj>)
                                    match t with
                                    | Option (Option t) -> <@@ makeValue t %%arg @@>
                                    | _ -> <@@ %%arg @@>)
                            (propertyNames, coercedArgs)
                            ||> List.map2 (fun name value -> <@@ { RecordProperty.Name = name; Value = %%value } @@>)
                        Expr.NewArray(typeof<RecordProperty>, baseConstructorArgs)
                    Expr.NewObject(recordCtor, [Expr.Value(name); properties])
                let constructorParams =
                    constructorProperties
                    |> List.map (fun (name, t) -> ProvidedParameter(name, t, ?optionalValue = if isOption t then Some null else None))
                [ProvidedConstructor(constructorParams, invoker)]
            else
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
                        Expr.NewObject(recordCtor, [Expr.Value(name); properties])
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

let preBuildProvidedRecordType(metadata : ProvidedTypeMetadata, baseType : Type option) =
    let baseType = Option.defaultValue typeof<RecordBase> baseType
    let name = metadata.Name.FirstCharUpper()
    let tdef = ProvidedRecordTypeDefinition(name, Some baseType)
    tdef
