module internal FSharp.Data.GraphQL.Server.AspNet.Json

open System
open System.Reflection
open System.Text.Json
open System.Text.Json.Serialization
open System.Collections.Concurrent

open FSharp.Reflection
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

type IJsonVariableReader =
    abstract IsNullable : bool
    abstract Read : replacements: Map<string, FileUpload> * path: string * element: JsonElement -> obj

type TypeReaderCache = ConcurrentDictionary<InputDef, IJsonVariableReader>

let (|SpecificTypeDef|_|) (def: TypeDef) (value: TypeDef) =
    if def = value then Some() else None

let combinePath (left: string) (right: string) =
    sprintf "%s.%s" left right

let toCamelCase (value: string) =
    JsonNamingPolicy.CamelCase.ConvertName value

let getOptionCtors (optionInnerType: Type) =
    let optionType = typedefof<option<_>>
    let cases = FSharpType.GetUnionCases(optionType.MakeGenericType(optionInnerType))
    let none = FSharpValue.PreComputeUnionConstructor cases.[0]
    let some = FSharpValue.PreComputeUnionConstructor cases.[1]
    some, none

let private getCallInfo = function
    | Quotations.Patterns.Call(_, info, _) -> info
    | q -> failwithf "Unexpected Quotation! %A" q

let convertArray (items: seq<obj>) : 'T array=
    items
    |> Seq.cast<'T>
    |> Array.ofSeq

let convertList (items: seq<obj>) : 'T list =
    items
    |> Seq.cast<'T>
    |> List.ofSeq

let convertSet (items: seq<obj>) : 'T Set =
    items
    |> Seq.cast<'T>
    |> Set.ofSeq

let convertSeq (items: seq<obj>) : 'T seq =
    items
    |> Seq.cast<'T>
    |> Array.ofSeq
    :> seq<_>

let rec getJsonReaderAux (input: InputDef) (cache: TypeReaderCache) =
    match input with
    | Nullable (Input(innerDef)) -> getNullableReader innerDef cache
    | Scalar scalarDef -> getScalarReader scalarDef
    | Enum enumDef -> getEnumReader enumDef
    | List(Input(elementDef)) -> getListReader input elementDef cache
    | InputObject inputObjectDef -> getInputObjectReader inputObjectDef cache
    | typeDef -> raise(invalidOp(sprintf "Invalid input definition '%s'" typeDef.Type.Name))
and getJsonReader (input: InputDef) (cache: ConcurrentDictionary<InputDef, IJsonVariableReader>) =
    match cache.TryGetValue input with
    | true, value ->
        value
    | false, _ ->
        let readerFunc = lazy getJsonReaderAux input cache
        let isNullable = input :? NullableDef
        cache.GetOrAdd(input, (fun input ->
            { new IJsonVariableReader with
                member _.IsNullable = isNullable
                member _.Read (replacements, path, element) =
                    readerFunc.Value replacements path element }))
and getNullableReader (innerDef: InputDef) (cache: TypeReaderCache) =
    let innerReader = getJsonReader innerDef cache
    let someCtor, noneCtor =  getOptionCtors innerDef.Type
    fun replacements path (element: JsonElement) ->
        match element.ValueKind with
        | JsonValueKind.Null ->
            match Map.tryFind path replacements with
            | Some file -> someCtor [| file :> obj |]
            | None -> noneCtor [||]
        | _ ->
            someCtor [| innerReader.Read(replacements, path, element) |]
and getScalarReader (scalarDef: ScalarDef) =
    match scalarDef with
    | SpecificTypeDef SchemaDefinitions.Date ->
        fun _ _ (element: JsonElement) -> element.GetDateTime() :> _
    | SpecificTypeDef SchemaDefinitions.Guid ->
        fun _ _ (element: JsonElement) -> element.GetGuid() :> _
    | SpecificTypeDef SchemaDefinitions.Uri ->
        fun _ _ (element: JsonElement) -> System.Uri(element.GetString()) :> _
    | SpecificTypeDef SchemaDefinitions.Boolean ->
        fun _ _ (element: JsonElement) -> element.GetBoolean() :> _
    | SpecificTypeDef SchemaDefinitions.Float ->
        fun _ _ (element: JsonElement) -> element.GetDouble() :> _
    | SpecificTypeDef SchemaDefinitions.Int ->
        fun _ _ (element: JsonElement) -> element.GetInt32() :> _
    | SpecificTypeDef SchemaDefinitions.FileUpload ->
        fun replacements path (element: JsonElement) ->
            match Map.tryFind path replacements with
            | Some file -> file :> _
            | None -> failwithf "Expected file upload at %s" path
    | _ ->
        fun _ _ (element: JsonElement) -> element.GetString() :> _
and getEnumReader (enumDef: EnumDef) =
    if enumDef.Type.IsEnum then
        fun _ _ element ->
           let value = element.GetString()
           Enum.Parse(enumDef.Type, value, ignoreCase=true)
    else
        let flags = BindingFlags.NonPublic ||| BindingFlags.Public
        let cases = FSharpType.GetUnionCases(enumDef.Type, flags)
        let ctors =
            cases
            |> Seq.map(fun case -> case.Name.ToLowerInvariant(), FSharpValue.PreComputeUnionConstructor case)
            |> Map.ofSeq
        fun _ _ element ->
            let value = element.GetString()
            match Map.tryFind (value.ToLowerInvariant()) ctors with
            | Some ctor -> ctor [||]
            | None -> failwithf "Case '%s' does not match any Union constructors in '%s'" value enumDef.Type.Name
and getListReader (listDef: InputDef) (elementDef: InputDef) (cache: TypeReaderCache) =
    if listDef.Type.IsGenericType then
        let elementReader = getJsonReader elementDef cache
        let genericType = listDef.Type.GetGenericTypeDefinition()
        let collectionConverterMethInfo =
            if genericType = typedefof<list<_>> then getCallInfo <@ convertList Seq.empty @>
            elif genericType = typedefof<array<_>> then getCallInfo <@  convertArray Seq.empty @>
            elif genericType = typedefof<Set<_>> then getCallInfo <@ convertSet Seq.empty @>
            else getCallInfo <@ convertSeq Seq.empty @>
        let genericMethInfo = collectionConverterMethInfo.GetGenericMethodDefinition()
        let methInfo = genericMethInfo.MakeGenericMethod([|elementDef.Type|])
        let convertF v = methInfo.Invoke(null, [|v|])
        fun replacements path element ->
            match element.ValueKind with
            | JsonValueKind.Array ->
                use enumerator = element.EnumerateArray()
                enumerator
                |> Seq.mapi(fun i value ->
                    let elementPath = combinePath path (i.ToString())
                    elementReader.Read(replacements, elementPath, value) )
                |> convertF
            | otherElement ->
                failwithf "Expected array element but received '%A'" otherElement
    else
        failwithf "Unsupported GraphQL 'List' type '%s'"  listDef.Type.Name
and getInputObjectReader (inputObject: InputObjectDef) (cache: TypeReaderCache) =
    if FSharpType.IsRecord inputObject.Type then
        let inputFieldReaders =
            inputObject.Fields
            |> Array.map (fun field -> field.Name.ToLowerInvariant(), (field, toCamelCase field.Name, getJsonReader field.TypeDef cache))
            |> Map.ofArray
        let fieldReaders =
            [ for field in FSharpType.GetRecordFields(inputObject.Type, true) do
                let fieldName = field.Name.ToLowerInvariant()
                match Map.tryFind fieldName inputFieldReaders with
                | Some fieldReader -> fieldReader
                | None -> failwithf "field '%s' exists on Record '%s' but not InputObject '%s'" field.Name inputObject.Type.Name inputObject.Name ]
        let ctor = FSharpValue.PreComputeRecordConstructor(inputObject.Type, true)
        fun replacements path element ->
            match element.ValueKind with
            | JsonValueKind.Object ->
                let values =
                    [| for (field, name, reader) in fieldReaders do
                         let elementPath = combinePath path name
                         match Map.tryFind elementPath replacements with
                         | Some value ->
                             box value
                         | None ->
                            match element.TryGetProperty name, field.DefaultValue with
                            | (true, value), _ ->
                                reader.Read(replacements, elementPath, value)
                            | (false, _), Some(defaultValue) ->
                                box defaultValue
                            | (false, _), None when reader.IsNullable ->
                                match field.TypeDef with
                                | Nullable(Input(innerDef)) ->
                                    let _, noneCtor =  getOptionCtors innerDef.Type
                                    noneCtor [||]
                                | _ ->
                                    failwithf "Expected field '%s' to be a nullable type" field.Name
                            | (false, _), None ->
                                failwithf "Field '%s' is required, but not present in json object" field.Name |]
                ctor values
            | fieldKind ->
                failwithf "Expected Json Object for InputObject '%s' but received '%A'" inputObject.Name fieldKind
    else
        failwithf "InputObject '%s' must be a Record type." inputObject.Type.Name

let readVariable (cache: TypeReaderCache) (var: VarDef) (basePath: string) (element: JsonElement) (replacements: Map<string, FileUpload>) =
    let path = combinePath basePath var.Name
    let reader = getJsonReader var.TypeDef cache
    let variableValue = reader.Read(replacements, path, element)
    (var.Name, variableValue)

let readVariables (cache: TypeReaderCache) (replacements: Map<string, FileUpload>) (index: int option) (variablesJson: JsonElement option) (vars: list<VarDef>) =
    let basePath =
        match index with
        | Some idx -> combinePath (idx.ToString()) "variables"
        | None -> "variables"
    let tryReadProperty =
        match variablesJson with
        | Some variables ->
            fun (name: string) ->
                match variables.TryGetProperty(name) with
                | true, value -> Some value
                | false, _ -> None
        | None ->
            fun _ -> None
    Map.ofList [
        for var in vars do
            match tryReadProperty var.Name with
            | Some element ->
                readVariable cache var basePath element replacements
            | None ->
                match var.DefaultValue, var.TypeDef with
                | Some _, _ -> ()
                | _, Nullable _ -> ()
                | _  -> failwithf "Variable '%s' is missing and there is no default value" var.Name
    ]

type NameValueLookupConverter () =
    inherit JsonConverter<NameValueLookup>()
    override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: System.Type, options: JsonSerializerOptions) =
        invalidOp "deserialization not supported"

    override _.Write(writer: Utf8JsonWriter, value: NameValueLookup, options: JsonSerializerOptions) =
        writer.WriteStartObject()
        for (KeyValue(key, data)) in value do
            writer.WritePropertyName(key)
            if isNull data then
                writer.WriteNullValue()
            else
                JsonSerializer.Serialize(writer, data, data.GetType(), options)
        writer.WriteEndObject()

type GQLResponseConverter () =
    inherit JsonConverter<GQLResponse>()

    override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: System.Type, options: JsonSerializerOptions) =
        invalidOp "deserialization not supported"

    override _.Write(writer: Utf8JsonWriter, value: GQLResponse, options: JsonSerializerOptions) =
        match value with
        | Direct(data, errors) ->
            JsonSerializer.Serialize(writer, data, typeof<NameValueLookup>, options)
        | _ ->
            writer.WriteNull("data")
