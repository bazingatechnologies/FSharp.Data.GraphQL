#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../packages/System.Runtime/lib/net462/System.Runtime.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Shared.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Server.dll"
#load "models.fsx"

open System
open System.Net
open System.IO
open System.Linq
open System.Collections.Generic
open Models

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json
open Newtonsoft.Json.Linq

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

let deepParse (json: string) : IDictionary<string, obj> = 
    let jobj = JToken.Parse(json)
    let rec parse (token:JToken) : obj = 
        match token.Type with
        | JTokenType.Object -> upcast (token.Children<JProperty>() |> Seq.map(fun x -> x.Name, parse x.Value) |> dict)
        | JTokenType.Array -> upcast (token |> Seq.map parse |> Seq.toList )
        | _ -> (token :?> JValue).Value

    (parse jobj) :?> IDictionary<string, obj>

let makeHttpReq (url: string) (odata: obj) = 
    let url = "http://localhost:8084"
    let req = WebRequest.CreateHttp url
    req.CookieContainer <- new CookieContainer()
    req.Method <- "POST"
    req.ProtocolVersion <- HttpVersion.Version10
    let postBytes = odata :?> string |> System.Text.Encoding.ASCII.GetBytes
    req.ContentLength <- postBytes.LongLength
    req.ContentType <- "application/json; charset=utf-8"
    async{
        use! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask
        do! reqStream.WriteAsync(postBytes, 0, postBytes.Length) |> Async.AwaitIAsyncResult |> Async.Ignore
        reqStream.Close()
        use! res = req.AsyncGetResponse() 
        use stream = res.GetResponseStream()
        use reader = new StreamReader(stream)
        let! rdata = reader.ReadToEndAsync() |> Async.AwaitTask
        return deepParse rdata
    }

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.RemoteSchema

let serializer o = JsonConvert.SerializeObject(o, settings) :> obj

let transport (o: obj): IDictionary<string, obj> = 
    upcast (makeHttpReq "http://localhost:8084" o |> Async.RunSynchronously )

let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String); Define.Field("friends", Obj) ])
let DogType =
  Define.Object<Models.Dog>(
    name = "Dog", 
    isTypeOf = (fun o -> o :? Models.Dog),
    interfaces = [ PetType ],
    fields = [
        Define.Field("name", String, fun _ d -> d.Name)
        Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
        Define.Field("weight", Int, fun _ d -> d.Weight)
        Define.RemoteSchemaField("friends", serializer, transport)
    ])
let CatType =
  Define.Object<Models.Cat>(
    name = "Cat", 
    isTypeOf = (fun o -> o :? Models.Cat),
    interfaces = [ PetType ],
    fields = [
        Define.Field("name", String, fun _ c -> c.Name)
        Define.Field("meows", Boolean, fun _ c -> c.Meows)
        Define.Field("weight", Int, fun _ d -> d.Weight)
        Define.Field("friends", Obj, fun _ d -> d.Friends :> obj)
    ])

let schema =
    Schema(
      query = Define.Object("Query", fun () ->
      [
         Define.Field("pets", ListOf PetType, (fun _ _ -> [ 
            { 
                Models.Dog.Name = "Odie";
                Models.Dog.Woofs = true;
                Models.Dog.Weight=10;
                Models.Dog.Friends=[|"bb"|]
            } :> Models.IPet ;
            {
                Models.Cat.Name = "Garfield";
                Models.Cat.Meows = false;
                Models.Cat.Weight=20;
                Models.Cat.Friends=[||]
            } :> Models.IPet ])) :> FieldDef<Models.IPet>
      ]), 
      config = { SchemaConfig.Default with Types = [CatType; DogType] })

let ex = Executor(schema)
    
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
            match tryParse "query" http.request.rawForm with
            | Some query ->
                printfn "Received query: %s" query
                // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                let q = query.Trim().Replace("\r\n", " ")
                let! result = ex.AsyncExecute(q)   
                return! http |> Successful.OK (json result)
            | None ->
                let! schemaResult = ex.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"


startWebServer defaultConfig (setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json")
// Example:
// curl --form 'query={ hero(id: "1000") { id, name, appearsIn, friends { id,name } } }' http://localhost:8083/