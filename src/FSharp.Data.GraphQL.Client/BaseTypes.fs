/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Globalization
open FSharp.Core
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Types.Introspection
open System.Text
open System.ComponentModel

/// Contains information about a field on the query.
type SchemaFieldInfo =
      /// Gets the alias or the name of the field.
    { AliasOrName : string
      /// Gets the introspection type information of the field.
      SchemaTypeRef : IntrospectionTypeRef
      /// Gets information about fields of this field, if it is an object type.
      Fields : SchemaFieldInfo [] }

/// A type alias to represent a Type name.
type TypeName = string

/// Contains data about a GQL operation error.
type OperationError =
      /// The description of the error that happened in the operation.
    { Message : string
      /// The path to the field that produced the error while resolving its value.
      Path : obj [] }

/// Contains helpers to build HTTP header sequences to be used in GraphQLProvider Run methods.
module HttpHeaders =
    /// Builds a sequence of HTTP headers as a sequence from a pre-formatted header string.
    /// The input headers string should be a string containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofString (headers : string) : seq<string * string> =
        upcast (headers.Replace("\r\n", "\n").Split('\n')
                |> Array.map (fun header -> 
                    let separatorIndex = header.IndexOf(':')
                    if separatorIndex = -1
                    then failwithf "Header \"%s\" has an invalid header format. Must provide a name and a value, both separated by a comma." header
                    else
                        let name = header.Substring(0, separatorIndex).Trim()
                        let value = header.Substring(separatorIndex + 1).Trim()
                        (name, value)))
        
    /// Builds a sequence of HTTP headers as a sequence from a header file.
    /// The input file should be a file containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofFile (path : string) =
        System.IO.File.ReadAllText path |> ofString

    let internal load (location : StringLocation) : seq<string * string> =
        let headersString =
            match location with
            | String headers -> headers
            | File path -> System.IO.File.ReadAllText path
        if headersString = "" then upcast [||]
        else headersString |> ofString

/// The base type for all GraphQLProvider provided enum types.
type EnumBase (name : string, value : string) =
    /// Gets the name of the provided enum type.
    member __.GetName() = name

    /// Gets the value of the provided enum type.
    member __.GetValue() = value

    override x.ToString() = x.GetValue()

    member x.Equals(other : EnumBase) =
        x.GetName() = other.GetName() && x.GetValue() = other.GetValue()

    override x.Equals(other : obj) =
        match other with
        | :? EnumBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.GetName().GetHashCode() ^^^ x.GetValue().GetHashCode()

    interface IEquatable<EnumBase> with
        member x.Equals(other) = x.Equals(other)

/// Contains information about a GraphQLProvider record property.
type RecordProperty =
      /// Gets the name of the record property.
    { Name : string
      /// Gets the value of the record property.
      Value : obj }

/// The base type for all GraphQLProvider provided record types.
type RecordBase (name : string, properties : RecordProperty seq) =
    do
        if not (isNull properties)
        then
            let distinctCount = properties |> Seq.map (fun p -> p.Name) |> Seq.distinct |> Seq.length
            if distinctCount <> Seq.length properties
            then failwith "Duplicated property names were found. Record can not be created, because each property name must be distinct."

    let properties = 
        if not (isNull properties)
        then properties |> Seq.sortBy (fun x -> x.Name) |> List.ofSeq
        else []

    /// Gets the name of this provided record type.
    member __.GetName() = name

    /// Gets a list of this provided record properties.
    member __.GetProperties() = properties
    
    /// Produces a dictionary containing all the properties of this provided record type.
    member x.ToDictionary() =
        let rec mapDictionaryValue (v : obj) =
            match v with
            | null -> null
            | :? string -> v // We need this because strings are enumerables, and we don't want to enumerate them recursively as an object
            | :? EnumBase as v -> v.GetValue() |> box
            | :? RecordBase as v -> box (v.ToDictionary())
            | OptionValue v -> v |> Option.map mapDictionaryValue |> Option.toObj
            | EnumerableValue v -> v |> Array.map mapDictionaryValue |> box
            | _ -> v
        x.GetProperties()
        |> Seq.choose (fun p -> 
            if not (isNull p.Value)
            then Some (p.Name, mapDictionaryValue p.Value)
            else None)
        |> dict

    override x.ToString() =
        let getPropValue (prop : RecordProperty) = sprintf "%A" prop.Value
        let sb = StringBuilder()
        sb.Append("{") |> ignore
        let rec printProperties (properties : RecordProperty list) =
            match properties with
            | [] -> ()
            | [prop] -> sb.Append(sprintf "%s = %s;" prop.Name (getPropValue prop)) |> ignore
            | prop :: tail -> sb.AppendLine(sprintf "%s = %s;" prop.Name (getPropValue prop)) |> ignore; printProperties tail
        printProperties (x.GetProperties())
        sb.Append("}") |> ignore
        sb.ToString()

    member x.Equals(other : RecordBase) = 
        x.GetName() = other.GetName() && x.GetProperties() = other.GetProperties()

    override x.Equals(other : obj) =
        match other with
        | :? RecordBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = 
        x.GetName().GetHashCode() ^^^ x.GetProperties().GetHashCode()

    interface IEquatable<RecordBase> with
        member x.Equals(other) = x.Equals(other)

module internal Types =
    let scalar =
        [| "Int", typeof<int>
           "Boolean", typeof<bool>
           "Date", typeof<DateTime>
           "Float", typeof<float>
           "ID", typeof<string>
           "String", typeof<string>
           "URI", typeof<Uri> |]
        |> Map.ofArray

    let getSchemaTypes (introspection : IntrospectionSchema) =
        let schemaTypeNames =
            [| "__TypeKind"
               "__DirectiveLocation"
               "__Type"
               "__InputValue"
               "__Field"
               "__EnumValue"
               "__Directive"
               "__Schema" |]
        let isScalarType (name : string) =
            scalar |> Map.containsKey name
        let isIntrospectionType (name : string) =
            schemaTypeNames |> Array.contains name
        introspection.Types
        |> Array.choose (fun t ->
            if not (isIntrospectionType t.Name) && not (isScalarType t.Name)
            then Some(t.Name, t)
            else None)
        |> Map.ofArray

    let mapScalarType uploadInputTypeName tname =
        match uploadInputTypeName with
        | Some uploadInputTypeName when uploadInputTypeName = tname -> typeof<Upload>
        | _ ->
            // Unknown scalar types will be mapped to a string type.
            if scalar.ContainsKey(tname)
            then scalar.[tname]
            else typeof<string>

    let makeOption (t : Type) = typedefof<_ option>.MakeGenericType(t)

    let makeArray (t : Type) = t.MakeArrayType()

    let unwrapOption (t : Type) = 
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        then t.GetGenericArguments().[0]
        else failwithf "Expected type to be an Option type, but it is %s." t.Name

    let makeAsync (t : Type) = typedefof<Async<_>>.MakeGenericType(t)

module internal JsonValueHelper =
    let getResponseFields (responseJson : JsonValue) =
        match responseJson with
        | JsonValue.Record fields -> fields
        | _ -> failwithf "Expected root type to be a Record type, but type is %A." responseJson

    let getResponseDataFields (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "data") with
        | Some (_, data) -> 
            match data with
            | JsonValue.Record fields -> Some fields
            | JsonValue.Null -> None
            | _ -> failwithf "Expected data field of root type to be a Record type, but type is %A." data
        | None -> None

    let getResponseErrors (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "errors") with
        | Some (_, errors) ->
            match errors with
            | JsonValue.Array [||] | JsonValue.Null -> None
            | JsonValue.Array items -> Some items
            | _ -> failwithf "Expected error field of root type to be an Array type, but type is %A." errors
        | None -> None

    let getResponseCustomFields (responseJson : JsonValue) =
        getResponseFields responseJson
        |> Array.filter (fun (name, _) -> name <> "data" && name <> "errors")

    let private removeTypeNameField (fields : (string * JsonValue) []) =
        fields |> Array.filter (fun (name, _) -> name <> "__typename")

    let firstUpper (name : string, value) =
        name.FirstCharUpper(), value

    let getTypeName (fields : (string * JsonValue) seq) =
        fields
        |> Seq.tryFind (fun (name, _) -> name = "__typename")
        |> Option.map (fun (_, value) ->
            match value with
            | JsonValue.String x -> x
            | _ -> failwithf "Expected \"__typename\" field to be a string field, but it was %A." value)

    let rec getFieldValue (schemaField : SchemaFieldInfo) (fieldName : string, fieldValue : JsonValue) =
        let getScalarType (typeRef : IntrospectionTypeRef) =
            let getType (typeName : string) =
                match Map.tryFind typeName Types.scalar with
                | Some t -> t
                | None -> failwithf "Unsupported scalar type \"%s\"." typeName
            match typeRef.Name with
            | Some name -> getType name
            | None -> failwith "Expected scalar type to have a name, but it does not have one."
        let rec helper (useOption : bool) (schemaField : SchemaFieldInfo) (fieldValue : JsonValue) : obj =
            let makeSomeIfNeeded value =
                match schemaField.SchemaTypeRef.Kind with
                | TypeKind.NON_NULL -> value
                | _ when useOption -> makeSome value
                | _ -> value
            let makeNoneIfNeeded (t : Type) =
                match schemaField.SchemaTypeRef.Kind with
                | TypeKind.NON_NULL -> null
                | _ when useOption -> makeNone t
                | _ -> null
            match fieldValue with
            | JsonValue.Array items ->
                let items =
                    let itemType =
                        let tref =
                            match schemaField.SchemaTypeRef.Kind with
                            | TypeKind.LIST -> schemaField.SchemaTypeRef.OfType
                            | TypeKind.NON_NULL ->
                                match schemaField.SchemaTypeRef.OfType with
                                | Some t when t.Kind = TypeKind.LIST -> t.OfType
                                | _ -> failwithf "Expected field to be a list type with an underlying item, but it is %A." schemaField.SchemaTypeRef.OfType
                            | _ -> failwithf "Expected field to be a list type with an underlying item, but it is %A." schemaField.SchemaTypeRef
                        match tref with
                        | Some t -> t
                        | None -> failwith "Schema type is a list type, but no underlying type was specified."
                    let items = 
                        let schemaField = { schemaField with SchemaTypeRef = itemType }
                        items |> Array.map (helper false schemaField)
                    match itemType.Kind with
                    | TypeKind.NON_NULL -> 
                        match itemType.OfType with
                        | Some itemType ->
                            match itemType.Kind with
                            | TypeKind.NON_NULL -> failwith "Schema definition is not supported: a non null type of a non null type was specified."
                            | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeArray typeof<RecordBase> items
                            | TypeKind.ENUM -> makeArray typeof<EnumBase> items
                            | TypeKind.SCALAR -> makeArray (getScalarType itemType) items
                            | kind -> failwithf "Unsupported type kind \"%A\"." kind
                        | None -> failwith "Item type is a non null type, but no underlying type exists on the schema definition of the type."
                    | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeOptionArray typeof<RecordBase> items
                    | TypeKind.ENUM -> makeOptionArray typeof<EnumBase> items
                    | TypeKind.SCALAR -> makeOptionArray (getScalarType itemType) items
                    | kind -> failwithf "Unsupported type kind \"%A\"." kind
                makeSomeIfNeeded items
            | JsonValue.Record props -> 
                let typeName =
                    match getTypeName props with
                    | Some typeName -> typeName
                    | None -> failwith "Expected type to have a \"__typename\" field, but it was not found."
                let mapRecordProperty (aliasOrName : string, value : JsonValue) =
                    let schemaField =
                        match schemaField.Fields |> Array.tryFind (fun f -> f.AliasOrName = aliasOrName) with
                        | Some f -> f
                        | None -> failwithf "Expected to find field information for field with alias or name \"%s\" of type \"%s\" but it was not found." aliasOrName typeName
                    let value = helper true schemaField value
                    { Name = aliasOrName; Value = value }
                let props =
                    props
                    |> removeTypeNameField
                    |> Array.map (firstUpper >> mapRecordProperty)
                RecordBase(typeName, props) |> makeSomeIfNeeded
            | JsonValue.Boolean b -> makeSomeIfNeeded b
            | JsonValue.Float f -> makeSomeIfNeeded f
            | JsonValue.Null ->
                match schemaField.SchemaTypeRef.Kind with
                | TypeKind.NON_NULL -> failwith "Expected a non null item from the schema definition, but a null item was found in the response."
                | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeNoneIfNeeded typeof<RecordBase>
                | TypeKind.ENUM -> makeNoneIfNeeded typeof<EnumBase>
                | TypeKind.SCALAR -> getScalarType schemaField.SchemaTypeRef |> makeNoneIfNeeded
                | TypeKind.LIST -> null
                | kind -> failwithf "Unsupported type kind \"%A\"." kind
            | JsonValue.Integer n -> makeSomeIfNeeded n
            | JsonValue.String s -> 
                match schemaField.SchemaTypeRef.Kind with
                | TypeKind.NON_NULL ->
                    match schemaField.SchemaTypeRef.OfType with
                    | Some itemType ->
                        match itemType.Kind with
                        | TypeKind.NON_NULL -> failwith "Schema definition is not supported: a non null type of a non null type was specified."
                        | TypeKind.SCALAR -> 
                            match itemType.Name with
                            | Some "String" | Some "ID" -> box s
                            | Some "URI" -> System.Uri(s) |> box
                            | Some "Date" -> 
                                match DateTime.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                                | (true, d) -> box d
                                | _ -> failwith "A string was received in the query response, and the schema recognizes it as a date and time string, but the conversion failed."
                            | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type."
                        | TypeKind.ENUM when itemType.Name.IsSome -> EnumBase(itemType.Name.Value, s) |> box
                        | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string or an enum type."
                    | None -> failwith "Item type is a non null type, but no underlying type exists on the schema definition of the type."
                | TypeKind.SCALAR ->
                    match schemaField.SchemaTypeRef.Name with
                    | Some "String" | Some "ID" -> makeSomeIfNeeded s
                    | Some "URI" -> System.Uri(s) |> makeSomeIfNeeded
                    | Some "Date" -> 
                        match DateTime.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                        | (true, d) -> makeSomeIfNeeded d
                        | _ -> failwith "A string was received in the query response, and the schema recognizes it as a date and time string, but the conversion failed."
                    | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type."
                | TypeKind.ENUM when schemaField.SchemaTypeRef.Name.IsSome -> EnumBase(schemaField.SchemaTypeRef.Name.Value, s) |> makeSomeIfNeeded
                | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type or an enum type."
        fieldName, (helper true schemaField fieldValue)

    let getFieldValues (schemaTypeName : string) (schemaFields : SchemaFieldInfo []) (dataFields : (string * JsonValue) []) =
        let mapFieldValue (aliasOrName : string, value : JsonValue) =
            let schemaField =
                match schemaFields |> Array.tryFind (fun f -> f.AliasOrName = aliasOrName) with
                | Some f -> f
                | None -> failwithf "Expected to find field information for field with alias or name \"%s\" of type \"%s\" but it was not found." aliasOrName schemaTypeName
            getFieldValue schemaField (aliasOrName, value)
        removeTypeNameField dataFields
        |> Array.map (firstUpper >> mapFieldValue)

    let getErrors (errors : JsonValue []) =
        let errorMapper = function
            | JsonValue.Record fields ->
                match fields |> Array.tryFind (fun (name, _) -> name = "message"), fields |> Array.tryFind (fun (name, _) -> name = "path") with
                | Some (_, JsonValue.String message), Some (_, JsonValue.Array path) ->
                    let pathMapper = function
                        | JsonValue.String x -> box x
                        | JsonValue.Integer x -> box x
                        | _ -> failwith "Error parsing response errors. A item in the path is neither a String or a Number."
                    { Message = message; Path = Array.map pathMapper path }
                | _ -> failwith "Error parsing response errors. Unsupported errors field format."
            | other -> failwithf "Error parsing response errors. Expected error to be a Record type, but it is %s." (other.ToString())
        Array.map errorMapper errors

/// The base type for all GraphQLProvider operation result provided types.
type OperationResultBase (responseJson : JsonValue, operationFields : SchemaFieldInfo [], operationTypeName : string) =
    let rawData = 
        let data = JsonValueHelper.getResponseDataFields responseJson
        match data with
        | Some [||] | None -> None
        | Some dataFields ->
            let fieldValues = JsonValueHelper.getFieldValues operationTypeName operationFields dataFields
            let props = fieldValues |> Array.map (fun (name, value) -> { Name = name; Value = value })
            Some (RecordBase(operationTypeName, props))

    let errors =
        let errors = JsonValueHelper.getResponseErrors responseJson
        match errors with
        | None -> [||]
        | Some errors -> JsonValueHelper.getErrors errors

    let customData = 
        JsonValueHelper.getResponseCustomFields responseJson
        |> Serialization.deserializeMap

    member private __.ResponseJson = responseJson

    /// [omit]
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This property is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    member __.RawData = rawData

    /// Gets all the errors returned by the operation on the server.
    member __.Errors = errors
    
    /// Gets all the custom data returned by the operation on server as a map of names and values.
    member __.CustomData = customData

    member x.Equals(other : OperationResultBase) =
        x.ResponseJson = other.ResponseJson

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.ResponseJson.GetHashCode()

/// The base type for al GraphQLProvider operation provided types.
type OperationBase (query : string) =
    /// Gets the query string of the operation.
    member __.Query = query