namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open Dahomey.Json.Serialization.Converters
open Newtonsoft.Json.Linq

[<AutoOpen>]
module private JsonHelpers =

    let readObjectAsStreang (reader: byref<Utf8JsonReader>) =
        use document = JsonDocument.ParseValue (&reader)
        document.RootElement.GetRawText();

[<Sealed>]
type NameValueLookupConverter() =
    inherit JsonConverter<NameValueLookup>()

    override __.CanConvert(t) = t = typeof<NameValueLookup>

    override __.Read(reader, _, options) =
        let dictionary = JsonSerializer.Deserialize<ReadOnlyDictionary<string, obj>>(&reader, options)
        NameValueLookup(dictionary :> KeyValuePair<string, obj> seq)

    override __.Write(writer, lookup, options) =
        let converter = InterfaceDictionaryConverter(options)
        converter.Write(writer, lookup.Buffer, options)


[<AutoOpen>]
module private GraphQLRequestFields =
    let [<Literal>] FIELD_Query = "query"
    let [<Literal>] FIELD_Variables = "variables"

[<Sealed>]
type GraphQLQueryConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<GraphQLQuery>()

    override __.CanConvert(t) = t = typeof<GraphQLQuery>

    override __.Write(_, _, _) = raise <| NotSupportedException()

    override __.Read(reader, _, options) =
        if reader.TokenType <> JsonTokenType.StartObject then raise <| JsonException ()
        let properties = Dictionary<string, string>(2)
        reader.Read () |> ignore
        while reader.TokenType <> JsonTokenType.EndObject do
            let propertyName = reader.GetString ()
            reader.Read () |> ignore
            match propertyName with
            | FIELD_Query -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | FIELD_Variables -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | _ -> ()
            reader.Read () |> ignore

        let query = properties.[FIELD_Query]
        let plan =
            match meta with
            | Some meta -> executor.CreateExecutionPlan(query, meta = meta)
            | None -> executor.CreateExecutionPlan(query)
        let varDefs = plan.Variables
        match varDefs with
        | [] -> { ExecutionPlan = plan; Variables = Map.empty }
        | vs ->
            let query = JObject.Parse(query)
            // For multipart requests, we need to replace some variables
            let vars = JObject.Parse(properties.[FIELD_Variables])
            // TODO: adjust path
            Map.iter(fun path rep -> vars.SelectToken(path).Replace(JObject.FromObject(rep))) replacements
            let variables =
                vs
                |> List.fold (fun (acc: Map<string, obj>)(vdef: VarDef) ->
                    match vars.TryGetValue(vdef.Name) with
                    | true, jval ->
                        let v =
                            match jval.Type with
                            | JTokenType.Null -> null
                            | JTokenType.String -> jval.ToString() :> obj
                            | _ -> JsonSerializer.Deserialize(jval.ToString(), vdef.TypeDef.Type, options)
                        Map.add (vdef.Name) v acc
                    | false, _  ->
                        match vdef.DefaultValue, vdef.TypeDef with
                        | Some _, _ -> acc
                        | _, Nullable _ -> acc
                        | None, _ -> failwithf "Variable %s has no default value and is missing!" vdef.Name) Map.empty
            { ExecutionPlan = plan; Variables = variables }

[<AutoOpen>]
module private GraphQLSubscriptionFields =
    let [<Literal>] FIELD_Type = "type"
    let [<Literal>] FIELD_Id = "id"
    let [<Literal>] FIELD_Payload = "payload"
    let [<Literal>] FIELD_Error = "error"

[<Sealed>]
type WebSocketClientMessageConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<WebSocketClientMessage>()

    override __.CanConvert(t) = t = typeof<WebSocketClientMessage>

    override __.Write(_, _, _) = raise <| NotSupportedException()

    override __.Read(reader, _, options) =
        if reader.TokenType <> JsonTokenType.StartObject then raise <| JsonException ()
        let properties = Dictionary<string, string>(2)
        reader.Read () |> ignore
        while reader.TokenType <> JsonTokenType.EndObject do
            let propertyName = reader.GetString ()
            reader.Read () |> ignore
            match propertyName with
            | FIELD_Type
            | FIELD_Id -> properties.Add (propertyName, reader.GetString ())
            | FIELD_Payload -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | _ -> ()
            reader.Read () |> ignore

        let typ = properties.["type"]
        match typ with
        | "connection_init" -> ConnectionInit
        | "connection_terminate" -> ConnectionTerminate
        | "start" ->
            let id =
                match properties.TryGetValue FIELD_Id with
                | true, value -> ValueSome value
                | false, _ -> ValueNone
            let payload =
                match properties.TryGetValue FIELD_Payload with
                | true, value -> ValueSome value
                | false, _ -> ValueNone
            match id, payload with
            | ValueSome id, ValueSome payload ->
                try
                    let queryConverter =
                        match meta with
                        | Some meta -> GraphQLQueryConverter(executor, replacements, meta) :> JsonConverter
                        | None -> GraphQLQueryConverter(executor, replacements) :> JsonConverter
                    let options' = Json.getSerializerOptions ([|queryConverter|])
                    let req = JsonSerializer.Deserialize<GraphQLQuery>(payload, options')
                    Start(id, req)
                with e -> ParseError(Some id, "Parse Failed with Exception: " + e.Message)
            | ValueNone, _ -> ParseError(None, "Malformed GQL_START message, expected id field but found none")
            | _, ValueNone -> ParseError(None, "Malformed GQL_START message, expected payload field but found none")
        | "stop" ->
            match properties.TryGetValue FIELD_Id with
            | true, id -> Stop(id)
            | false, _ -> ParseError(None, "Malformed GQL_STOP message, expected id field but found none")
        | typ -> ParseError(None, $"Message Type {typ} is not supported!")

[<Sealed>]
type WebSocketServerMessageConverter() =
    inherit JsonConverter<WebSocketServerMessage>()

    override __.CanConvert(t) = t = typedefof<WebSocketServerMessage> || t.DeclaringType = typedefof<WebSocketServerMessage>

    override __.Read(_, _, _) = raise <| NotSupportedException()

    override __.Write(writer, value, options) =
        writer.WriteStartObject()
        match value with
        | ConnectionAck ->
            writer.WriteString(FIELD_Type, "connection_ack")
        | ConnectionError(err) ->
            writer.WriteString(FIELD_Type, "connection_error")
            writer.WritePropertyName(FIELD_Payload)
            writer.WriteStartObject()
            writer.WriteString(FIELD_Error, err)
            writer.WriteEndObject()
        | Error(id, err) ->
            writer.WriteString(FIELD_Type, "error")
            writer.WritePropertyName(FIELD_Payload)
            writer.WriteStartObject()
            writer.WriteString(FIELD_Error, err)
            writer.WriteEndObject()
            match id with
            | Some id -> writer.WriteString (FIELD_Id, id)
            | None -> writer.WriteNull(FIELD_Id)
        | Data(id, result) ->
            writer.WriteString(FIELD_Type, "data")
            writer.WriteString(FIELD_Id, id)
            writer.WritePropertyName(FIELD_Payload)
            JsonSerializer.Serialize(writer, result, options)
        | Complete(id) ->
            writer.WriteString(FIELD_Type, "complete")
            writer.WriteString(FIELD_Id, id)
        writer.WriteEndObject()
