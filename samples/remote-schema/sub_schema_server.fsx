#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../packages/System.Runtime/lib/net462/System.Runtime.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Shared.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Server.dll"
#load "models.fsx"

open System
open Models

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json
open FSharp.Data.GraphQL.RemoteSchema

type OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) = failwith "Not supported"
    
let settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() :> JsonConverter |]
settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
let json o = JsonConvert.SerializeObject(o, settings)


// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let FriendType = Define.Object<Models.Friend>(
        name = "Friend",
        isTypeOf = (fun o -> o :? Models.Friend),
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("weight", Int, fun _ d -> d.Weight)
        ])

let schema =
    Schema<obj>(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("friends", ListOf FriendType, (fun _ _ -> [{ Models.Friend.Name = "friend 1";  Models.Friend.Weight=10; } :> Models.Friend ; upcast { Models.Friend.Name = "friend 2";  Models.Friend.Weight=20; } ]))
        ]), 
        config = { SchemaConfig.Default with Types = [FriendType] })

let ex = Executor(schema)

let rsExecutor = RequestSchemaExecutor()
rsExecutor.RegisterSchema("friends", schema)
    
let tryParse fieldName data =
    let raw = Text.Encoding.UTF8.GetString data
    if raw <> null && raw <> ""
    then
        let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
        Map.tryFind fieldName map
    else None

let graphiql : WebPart =
    fun http ->
        async {
            let req = Text.Encoding.UTF8.GetString http.request.rawForm 
            //Console.WriteLine(req)
            let deSerializedData = JsonConvert.DeserializeObject<RemoteSchemaData>(Text.Encoding.UTF8.GetString http.request.rawForm)
            let result = rsExecutor.Execute(deSerializedData)
            return! http |> Successful.OK (json result)
        }

let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"

let local = Suave.Http.HttpBinding.mkSimple HTTP "127.0.0.1" 8084
let config = { defaultConfig with bindings = [local]}

startWebServer config (setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json")